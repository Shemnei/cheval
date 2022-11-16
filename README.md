# Cheval - An Arch Linux mirrorlist generator

## Example

```bash
cheval --https --ipv4 -c us -c au -c gb --sort score --take 10 -o /etc/pacman.d/mirrorlist
```

## Features

Order of operation:

1) Filter
2) Score && Sort
3) Take

### Filter

Filter mirrors by some criteria.

- Country
- HTTP/HTTPS capability
- IPv4/IPv6 capability

### Score

Score and then sort mirrors.

- Score given by the [Mirror Status Page](https://archlinux.org/mirrors/status/). Lower is better.
- Round trip time for a http get request

### Take

Limit the output to the first/best n mirrors.

## Misc

### Refresh

The mirrorlist is cached between uses if it is not older then `cache_secs` (default: 3600 secs = 1h).
To force a refresh of the list call the tool with the argument `--cache-secs 0`.

## Alternatives/Inspirations

- [reflector](https://xyne.archlinux.ca/projects/reflector/)

## Links

- [Mirror Status](https://archlinux.org/mirrors/status/)
- [Mirror Status json Interface](https://archlinux.org/mirrors/status/json/)
