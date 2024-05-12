

# Testing

    b0 test

# Testing performance

    time $(b0 --path -- zstdtrip -d) < file.zst > /dev/null
    time zstd -c -d < file.zst > /dev/null
    
    time $(b0 --path -- gziptrip -d) < file.gz > /dev/null
    time gunzip -c < file.gz > /dev/null
    
    time $(b0 --path -- xxh3tap --sink) < file 
    time xxhsum -H3 < file

    time $(b0 --path -- blake3tap --sink) < file 
    time b3sum < file
