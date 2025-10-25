This project uses (perhaps the development version of) [`b0`] for
development. Consult [b0 occasionally] for quick hints on how to
perform common development tasks.

[`b0`]: https://erratique.ch/software/b0
[b0 occasionally]: https://erratique.ch/software/b0/doc/occasionally.html

# Testing

    b0 test

# Benchmarking

    hyperfine "$(b0 --path -- zstdtrip) -d < tmp/webster.zst > /dev/null"
    hyperfine 'zstd -c -d < tmp/webster.zst > /dev/null'
    
    hyperfine "$(b0 --path -- gziptrip) -d < tmp/webster.gz > /dev/null"
    hyperfine 'gunzip -c < tmp/webster.gz > /dev/null'
    
    hyperfine "$(b0 --path -- xxh3tap) --sink < tmp/webster"
    hyperfine 'xxhsum -H3 < tmp/webster'

    hyperfine "$(b0 --path -- blake3tap) --sink < tmp/webster"
    hyperfine 'b3sum < tmp/webster'
