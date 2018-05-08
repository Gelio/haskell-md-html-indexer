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

## example headings
