# Testing

    b0 test

# Benchmarking

    hyperfine '$(b0 --path -- zstdtrip) -d < file.zst > /dev/null'
    hyperfine 'zstd -c -d < file.zst > /dev/null'
    
    hyperfine '$(b0 --path -- gziptrip) -d < file.gz > /dev/null'
    hyperfine 'gunzip -c < file.gz > /dev/null'
    
    hyperfine '$(b0 --path -- xxh3tap) --sink < file'
    hyperfine 'xxhsum -H3 < file'

    hyperfine '$(b0 --path -- blake3tap' --sink < file'
    hyperfine 'b3sum < file'
