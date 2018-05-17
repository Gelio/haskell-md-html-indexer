# md-html-indexer

## Building

```bash
stack build
```

## Running

```bash
stack exec md-html-indexer index example.html README.md README.md.gz http://w3schools.com
stack exec md-html-indexer search heading
stack exec md-html-indexer search Server
stack exec md-html-indexer -- search headings -e "ls -al {}"
```

## Formatting

```bash
hfmt src -w
```

## Linting

```bash
hlint src
```

## Example commands to test for concurrency

### Test for not interleaved output

`index.idx` should have a correct structure. The output cannot be interleaved.

```bash
stack exec md-html-indexer -- index https://stackoverflow.com/questions/857653/get-a-list-of-urls-from-a-site http://hackage.haskell.org/package/base-4.11.1.0/docs/System-IO-Unsafe.html https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Concurrent-MVar.html http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Concurrent.html http://hackage.haskell.org/package/parallel-3.1.0.1/docs/Control-Parallel-Strategies.html http://hackage.haskell.org/package/deepseq-1.4.3.0/docs/Control-DeepSeq.html#v:force http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html http://hackage.haskell.org/package/stm-conduit-4.0.0/docs/Data-Conduit-Async.html https://docs.expo.io/versions/latest/guides/push-notifications https://facebook.github.io/react-native/docs/asyncstorage.html#getitem https://www.abeautifulsite.net/downloading-a-list-of-urls-automatically https://developers.google.com/search/docs/guides/create-URLs https://developers.google.com/search/docs/guides/enhance-site https://en.wikipedia.org/wiki/Bandwidth_throttling
```

### Test for parallelism

The last two pages should be indexed (almost) immediately.

```bash
stack exec md-html-indexer -- index https://en.wikipedia.org/wiki/Bandwidth_throttling https://httpstat.us/200?sleep=10000  https://developers.google.com/search/docs/guides/enhance-site https://httpstat.us/200?sleep=1000
```
