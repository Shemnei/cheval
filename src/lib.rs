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
use std::fmt;
use std::time::{Duration, SystemTime};

use celes::Country;
use chrono::prelude::*;
use rayon::prelude::*;

/// TODO:
/// - cleanup
/// - logging
/// - error type(s)
/// - expose all filter to cli
/// - option for custom cache dir

/// <https://archlinux.org/mirrors/status/>
/// <https://archlinux.org/mirrors/status/json/>
/// <https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes>

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

	pub fn country(&self) -> &str {
		&self.country
	}
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MirrorList {
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

impl MirrorList {
	pub fn urls(&self) -> &[Mirror] {
		&self.urls
	}
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
				.map(|c| {
					c.as_ref().parse().map(|c: Country| c.alpha2.to_string())
				})
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
			Self::Country(country) => {
				fill_dbg!("Country": country)
			}
			Self::Protocol(protocol) => {
				fill_dbg!("Protocol": protocol)
			}
			Self::Active(active) => {
				fill_dbg!("Active": active)
			}
			Self::Score(ordering, score) => {
				fill_dbg!("Score": ordering, score)
			}
			Self::CompletionPercent(ordering, percent) => {
				fill_dbg!("CompletionPercent": ordering, percent)
			}
			Self::RefreshDelay(ordering, delay) => {
				fill_dbg!("RefreshDelay": ordering, delay)
			}
			Self::MeanDuration(ordering, duration) => {
				fill_dbg!("MeanDuration": ordering, duration)
			}
			Self::StddevDuration(ordering, duration) => {
				fill_dbg!("StddevDuration": ordering, duration)
			}
			Self::Ipv4(addr) => {
				fill_dbg!("Ipv4": addr)
			}
			Self::Ipv6(addr) => {
				fill_dbg!("Ipv6": addr)
			}
			Self::Isos(isos) => {
				fill_dbg!("Isos": isos)
			}
			Self::Custom(_) => {
				let func = &"...";
				fill_dbg!("Custom": func)
			}
			Self::Combine(combine) => {
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
