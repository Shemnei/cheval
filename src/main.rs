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

use std::ffi::OsString;
use std::io::Write as _;
use std::path::Path;
use std::str::FromStr;
use std::time::SystemTime;

use celes::Country;
use cheval::{MirrorFilter, MirrorList, MirrorScore};
use chrono::Local;
use structopt::StructOpt;

const DEFAULT_STATUS_URL: &str = "https://archlinux.org/mirrors/status/json/";

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

	let list: MirrorList = serde_json::from_str(&status_json)?;

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
			countries.into_iter().map(|c| c.alpha2.to_string()).collect(),
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

	let filtered = filter.filtered(&list.urls().iter().collect::<Vec<_>>());

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
			mirror.country(),
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

		write.write_all(output.as_bytes())?;
		write.flush()?;
	} else {
		let mut write = std::io::stdout();
		// remove leading newline
		write.write_all(output.trim().as_bytes())?;
		write.flush()?;
	};

	Ok(())
}
