#![allow(rustdoc::private_intra_doc_links)]
#![deny(
    // Documentation
	// TODO: rustdoc::broken_intra_doc_links,
	// TODO: rustdoc::missing_crate_level_docs,
	// TODO: missing_docs,
	// TODO: clippy::missing_docs_in_private_items,

    // Other
	deprecated_in_future,
	exported_private_dependencies,
	future_incompatible,
	missing_copy_implementations,
	missing_debug_implementations,
	private_in_public,
	rust_2018_compatibility,
	rust_2018_idioms,
	trivial_casts,
	trivial_numeric_casts,
	unsafe_code,
	unstable_features,
	unused_import_braces,
	unused_qualifications,

	// clippy attributes
	clippy::missing_const_for_fn,
	clippy::redundant_pub_crate,
	clippy::use_self
)]
#![cfg_attr(docsrs, feature(doc_cfg), feature(doc_alias))]

use std::borrow::Cow;
use std::cmp::Ordering;
use std::ffi::OsString;
use std::fmt;
use std::io::Write;
use std::path::Path;
use std::str::FromStr;
use std::time::{Duration, SystemTime};

use celes::Country;
use chrono::prelude::*;
use rayon::prelude::*;
use structopt::StructOpt;

/// TODO:
/// - cleanup
/// - logging
/// - error type(s)
/// - expose all filter to cli
/// - option for custom cache dir

/// <https://archlinux.org/mirrors/status/>
/// <https://archlinux.org/mirrors/status/json/>
/// <https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes>

const DEFAULT_STATUS_URL: &str = "https://archlinux.org/mirrors/status/json/";

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Mirror {
	/// Mirrors are checked on a per-URL basis.
	/// All available URLs and protocols for each known mirror are listed.
	url: String,

	/// Protocol used by the mirror, e.g. `https`, `http` or `rsync`.
	/// Only contains one value.
	protocol: String,

	/// UTC timestamp of last sync operation (sync with tier 1 mirror or root).
	last_sync: Option<DateTime<Utc>>,

	/// The number of mirror checks that have successfully connected and
	/// disconnected from the given URL.
	/// If this is below 100%, the mirror may be unreliable.
	completion_pct: f32,

	///  The calculated average mirroring delay;
	///  e.g. the mean value of last check − last sync for each check of this
	///  mirror URL. Due to the timing of mirror checks,
	///  any value under one hour should be viewed as ideal.
	delay: Option<u64>,

	/// The average (mean) time it took to connect and retrieve the lastsync
	/// file from the given URL.
	/// Note that this connection time is from the location of the Arch server;
	/// your geography may product different results.
	duration_avg: Option<f32>,

	/// The standard deviation of the connect and retrieval time.
	/// A high standard deviation can indicate an unstable or overloaded mirror.
	duration_stddev: Option<f32>,

	/// A very rough calculation for ranking mirrors.
	/// It is currently calculated as
	/// `(hours delay + average duration + standard deviation) / completion percentage`.
	/// Lower is better.
	score: Option<f32>,

	/// Indicates if the mirror is active.
	active: bool,

	/// Full country name of the mirrors location, e.g. `Bulgaria`.
	country: String,

	/// Alpha-2 code for the country (ISO-3166 Standard), e.g. `BG`.
	country_code: String,

	/// Indicates if the mirror is also hosting iso image downloads.
	isos: bool,

	/// Indicates if the mirror supports ipv4 addresses.
	ipv4: bool,

	/// Indicates if the mirror supports ipv6 addresses.
	ipv6: bool,

	/// URL to a detailed list of the ping requests/respones.
	details: String,
}

impl Mirror {
	pub fn ping_url(&self) -> String {
		format!("{}lastsync", self.url)
	}

	pub fn mirror_url(&self) -> String {
		format!("{}$repo/os/$arch", self.url)
	}

	pub fn mirrorlist_line(&self) -> String {
		format!("Server = {}", self.mirror_url())
	}
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Mirrorlist {
	/// Cutoff time in seconds for `num_checks`.
	cutoff: u64,

	/// UTC timestamp of the last complete mirror check.
	last_check: DateTime<Utc>,

	/// Number of checks ran in the last 24 hours.
	num_checks: u64,

	/// Check interval in seconds.
	check_frequency: u64,

	/// List of all checked/registerd mirrors.
	urls: Vec<Mirror>,

	/// Version of the json file.
	version: u64,
}

pub trait Filter {
	fn accept(&self, mirror: &Mirror) -> bool;
}

#[derive(Debug)]
pub enum MirrorFilterCombine {
	And(MirrorFilter, MirrorFilter),
	Or(MirrorFilter, MirrorFilter),
}

impl Filter for MirrorFilterCombine {
	fn accept(&self, mirror: &Mirror) -> bool {
		match self {
			Self::And(a, b) => a.accept(mirror) && b.accept(mirror),
			Self::Or(a, b) => a.accept(mirror) || b.accept(mirror),
		}
	}
}

pub enum MirrorFilter {
	Country(Vec<String>),
	Protocol(Vec<Cow<'static, str>>),
	Active(bool),
	Score(Ordering, f32),
	CompletionPercent(Ordering, f32),
	RefreshDelay(Ordering, Duration),
	MeanDuration(Ordering, Duration),
	StddevDuration(Ordering, Duration),
	Ipv4(bool),
	Ipv6(bool),
	Isos(bool),
	Custom(Box<dyn Fn(&Mirror) -> bool + Sync>),
	Combine(Box<MirrorFilterCombine>),
}

impl MirrorFilter {
	pub fn countries<
		C: AsRef<str>,
		I: Iterator<Item = C>,
		II: IntoIterator<Item = C, IntoIter = I>,
	>(
		countries: II,
	) -> Result<Self, String> {
		Ok(Self::Country(
			countries
				.into_iter()
				.map(|c| c.as_ref().parse().map(|c: Country| c.alpha2))
				.collect::<Result<_, _>>()?,
		))
	}

	pub fn rsync() -> Self {
		Self::Protocol(vec![Cow::Borrowed("rsync")])
	}

	pub fn http() -> Self {
		Self::Protocol(vec![Cow::Borrowed("https")])
	}

	pub fn https() -> Self {
		Self::Protocol(vec![Cow::Borrowed("https")])
	}

	pub const fn active() -> Self {
		Self::Active(true)
	}

	pub const fn inactive() -> Self {
		Self::Active(false)
	}

	pub const fn ipv4() -> Self {
		Self::Ipv4(true)
	}

	pub const fn ipv6() -> Self {
		Self::Ipv6(true)
	}

	pub fn and(self, other: Self) -> Self {
		Self::Combine(Box::new(MirrorFilterCombine::And(self, other)))
	}

	pub fn or(self, other: Self) -> Self {
		Self::Combine(Box::new(MirrorFilterCombine::Or(self, other)))
	}

	pub fn filtered<'a, 'b>(
		&self,
		mirrors: &'a [&'b Mirror],
	) -> Vec<&'b Mirror> {
		mirrors
			.par_iter()
			.copied()
			.filter(|mirror| self.accept(mirror))
			.collect()
	}
}

impl fmt::Debug for MirrorFilter {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
		let mut dbg_struct = f.debug_struct("MirrorFilter");

		macro_rules! fill_dbg {
			( $kind:literal : $( $value:ident ),+ ) => {
				dbg_struct
					.field("kind", &$kind)
					$( .field(stringify!($value), $value) )+
			};
		}

		match self {
			MirrorFilter::Country(country) => {
				fill_dbg!("Country": country)
			}
			MirrorFilter::Protocol(protocol) => {
				fill_dbg!("Protocol": protocol)
			}
			MirrorFilter::Active(active) => {
				fill_dbg!("Active": active)
			}
			MirrorFilter::Score(ordering, score) => {
				fill_dbg!("Score": ordering, score)
			}
			MirrorFilter::CompletionPercent(ordering, percent) => {
				fill_dbg!("CompletionPercent": ordering, percent)
			}
			MirrorFilter::RefreshDelay(ordering, delay) => {
				fill_dbg!("RefreshDelay": ordering, delay)
			}
			MirrorFilter::MeanDuration(ordering, duration) => {
				fill_dbg!("MeanDuration": ordering, duration)
			}
			MirrorFilter::StddevDuration(ordering, duration) => {
				fill_dbg!("StddevDuration": ordering, duration)
			}
			MirrorFilter::Ipv4(addr) => {
				fill_dbg!("Ipv4": addr)
			}
			MirrorFilter::Ipv6(addr) => {
				fill_dbg!("Ipv6": addr)
			}
			MirrorFilter::Isos(isos) => {
				fill_dbg!("Isos": isos)
			}
			MirrorFilter::Custom(_) => {
				let func = &"...";
				fill_dbg!("Custom": func)
			}
			MirrorFilter::Combine(combine) => {
				fill_dbg!("Combine": combine)
			}
		};

		dbg_struct.finish()
	}
}

impl Filter for MirrorFilter {
	fn accept(&self, mirror: &Mirror) -> bool {
		match self {
			Self::Country(countries) => {
				countries.iter().any(|c| c == &mirror.country_code)
			}
			Self::Protocol(protocols) => {
				protocols.iter().any(|p| p == &mirror.protocol)
			}
			Self::Active(active) => active == &mirror.active,
			Self::Score(cmp, value) => mirror
				.score
				.map_or(false, |delay| delay.partial_cmp(value) == Some(*cmp)),
			Self::CompletionPercent(cmp, value) => {
				mirror.completion_pct.partial_cmp(value) == Some(*cmp)
			}
			Self::RefreshDelay(cmp, value) => mirror
				.delay
				.map_or(false, |delay| &delay.cmp(&value.as_secs()) == cmp),
			Self::MeanDuration(cmp, value) => {
				mirror.duration_avg.map_or(false, |delay| {
					delay.partial_cmp(&value.as_secs_f32()) == Some(*cmp)
				})
			}
			Self::StddevDuration(cmp, value) => {
				mirror.duration_stddev.map_or(false, |delay| {
					delay.partial_cmp(&value.as_secs_f32()) == Some(*cmp)
				})
			}
			Self::Ipv4(ipv4) => ipv4 == &mirror.ipv4,
			Self::Ipv6(ipv6) => ipv6 == &mirror.ipv6,
			Self::Isos(isos) => isos == &mirror.isos,
			Self::Custom(f) => f(mirror),
			Self::Combine(c) => c.accept(mirror),
		}
	}
}

pub trait Score {
	fn score(&self, mirror: &Mirror) -> Option<f32>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirrorScore {
	Score,
	GetLastSync,
}

impl MirrorScore {
	pub fn scored<'a, 'b>(
		&self,
		mirrors: &'a [&'b Mirror],
	) -> Vec<(Option<f32>, &'b Mirror)> {
		mirrors
			.par_iter()
			.copied()
			.map(|mirror| (self.score(mirror), mirror))
			.collect()
	}

	pub fn scored_sorted<'a, 'b>(
		&self,
		mirrors: &'a [&'b Mirror],
	) -> Vec<(Option<f32>, &'b Mirror)> {
		let mut scored = self.scored(mirrors);
		scored.sort_by(|(score_a, _), (score_b, _)| {
			// Sorts nulls last and the rest accoring to the scores ordering
			match (score_a, score_b) {
				(None, None) => None,
				(None, Some(_)) => Some(Ordering::Greater),
				(Some(_), None) => Some(Ordering::Less),
				(Some(a), Some(b)) => a.partial_cmp(b),
			}
			.unwrap_or(Ordering::Equal)
		});
		scored
	}
}

impl Score for MirrorScore {
	fn score(&self, mirror: &Mirror) -> Option<f32> {
		match self {
			Self::Score => mirror.score,
			Self::GetLastSync => Pinger.ping(mirror).map(|d| d.as_secs_f32()),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pinger;

impl Pinger {
	pub fn ping(&self, mirror: &Mirror) -> Option<Duration> {
		let ping_url = mirror.ping_url();
		println!("Ping: {}", ping_url);
		let start = SystemTime::now();
		reqwest::blocking::get(&ping_url).ok()?;
		start.elapsed().ok()
	}
}

#[derive(Debug, StructOpt)]
pub struct Opt {
	/// URL for the mirror status json endpoint.
	#[structopt(short, long, default_value = DEFAULT_STATUS_URL)]
	url: String,

	/// Amount of secons a cached status file is valid.
	#[structopt(long, default_value = "3600")]
	cache_secs: u64,

	/// List of countries either as full name, alpha2 or alpha3 iso code.
	///
	/// The case of the input does not matter.
	/// Examples: `United States`, `US`, `USA`.
	#[structopt(short, long)]
	countries: Option<Vec<Country>>,

	/// Mirror must support http.
	#[structopt(long)]
	http: bool,

	/// Mirror must support https.
	#[structopt(long)]
	https: bool,

	/// Mirror must support ipv4 addresses.
	#[structopt(short = "4", long)]
	ipv4: bool,

	/// Mirror must support ipv6 addresses.
	#[structopt(short = "6", long)]
	ipv6: bool,

	/// Sort the mirrors after filtering.
	#[structopt(short, long)]
	sort: Option<Sorter>,

	/// Only take the best/first n mirrors after filtering and sorting.
	#[structopt(short, long)]
	take: Option<usize>,

	/// The generated mirrorlist will be written to this output.
	#[structopt(short, long)]
	output: Option<OsString>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Sorter {
	/// Ping the mirror and sort by fastest response.
	Ping,
	/// Sort by best score (taken from the mirror status page).
	Score,
}

impl FromStr for Sorter {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s.to_lowercase().as_str() {
			"ping" => Ok(Self::Ping),
			"score" => Ok(Self::Score),
			_ => {
				Err("Invalid sorter, allowed values are `score` and `ping`"
					.into())
			}
		}
	}
}

const ENV_CACHE_DIR: &str = "XDG_CACHE_HOME";
const CACHE_NAME: &str = "mirrorstatus.cheval";
const HEADER: &str = "##########
## Created by cheval
## Date: <DATE>
## Cmd: <CMD>
##########
";

fn request_mirror_status_json(
	url: &str,
) -> Result<String, Box<dyn std::error::Error>> {
	reqwest::blocking::get(url)?.text().map_err(|err| err.into())
}

fn load_cached(cache_secs: u64) -> Result<String, Box<dyn std::error::Error>> {
	if let Some(cache_dir) = xdglib::xdg_cache_home() {
		let cached_path = Path::new(&cache_dir).join(CACHE_NAME);
		let metadata = cached_path.metadata()?;
		if SystemTime::now().duration_since(metadata.modified()?)?.as_secs()
			<= cache_secs
		{
			std::fs::read_to_string(cached_path).map_err(|err| err.into())
		} else {
			Err(Box::new(std::io::Error::new(
				std::io::ErrorKind::InvalidData,
				"cache expired",
			)))
		}
	} else {
		Err(Box::new(std::io::Error::new(
			std::io::ErrorKind::NotFound,
			format!(
				"${} not set, mirrorlists will not be cached",
				ENV_CACHE_DIR
			),
		)))
	}
}

fn write_cache(json: &str) -> Result<(), std::io::Error> {
	let cache_dir = xdglib::xdg_cache_home().ok_or_else(|| {
		std::io::Error::new(
			std::io::ErrorKind::NotFound,
			format!(
				"${} not set, mirrorlists will not be cached",
				ENV_CACHE_DIR
			),
		)
	})?;
	std::fs::write(Path::new(&cache_dir).join(CACHE_NAME), json)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
	//Opt::clap().gen_completions(env!("CARGO_PKG_NAME"), structopt::clap::Shell::Zsh, "target");

	let opt = Opt::from_args();

	let (cache, status_json) = match load_cached(opt.cache_secs) {
		Ok(json) => (true, json),
		Err(err) => {
			eprintln!("CACHE_ERROR[R]: {}", err);
			(false, request_mirror_status_json(&opt.url)?)
		}
	};

	let list: Mirrorlist = serde_json::from_str(&status_json)?;

	// Only write cache if its a new json
	if !cache {
		// DO after convertion to list so that only a valid json will be written.
		if let Err(err) = write_cache(&status_json) {
			eprintln!("CACHE_ERROR[W]: {}", err);
		};
	}

	let mut filter = MirrorFilter::active();

	if let Some(countries) = opt.countries {
		filter = filter.and(MirrorFilter::Country(
			countries.into_iter().map(|c| c.alpha2).collect(),
		));
	}

	if opt.http {
		filter = filter.and(MirrorFilter::ipv4());
	}

	if opt.https {
		filter = filter.and(MirrorFilter::https());
	}

	if opt.ipv4 {
		filter = filter.and(MirrorFilter::ipv4());
	}

	if opt.ipv6 {
		filter = filter.and(MirrorFilter::ipv6());
	}

	let filtered = filter.filtered(&list.urls.iter().collect::<Vec<_>>());

	let sorted = if let Some(sort) = opt.sort {
		match sort {
			Sorter::Ping => MirrorScore::GetLastSync.scored_sorted(&filtered),
			Sorter::Score => MirrorScore::Score.scored_sorted(&filtered),
		}
	} else {
		filtered.into_iter().map(|mirror| (None, mirror)).collect()
	};

	let mirrors_out = if let Some(take) = opt.take {
		let len = std::cmp::min(sorted.len(), take);
		&sorted[..len]
	} else {
		&sorted
	};

	let mut output = String::new();

	for (score, mirror) in mirrors_out {
		let score_str = if let Some(score) = score {
			format!(" Score: {:.02}", score)
		} else {
			"".into()
		};

		output.push_str(&format!(
			"\n## Country: '{}'{}\nServer = {}",
			mirror.country,
			score_str,
			mirror.mirror_url()
		));
	}

	if let Some(output_file) = &opt.output {
		let mut write = std::fs::OpenOptions::new()
			.write(true)
			.truncate(true)
			.create(true)
			.open(output_file)?;

		let _ = write.write(
			HEADER
				.replacen("<DATE>", &Local::now().to_rfc3339(), 1)
				.replacen(
					"<CMD>",
					&std::env::args()
						.into_iter()
						.collect::<Vec<_>>()
						.join(" "),
					1,
				)
				.as_bytes(),
		)?;

		let _ = write.write(output.as_bytes())?;
		let _ = write.flush()?;
	} else {
		let mut write = std::io::stdout();
		// remove leading newline
		let _ = write.write(output.trim().as_bytes())?;
		let _ = write.flush()?;
	};

	Ok(())
}
