# feeder

A humble RSS/Atom feed reader hobby project.

## Prerequisites

You need Haskell and cabal installed. You also need a postgres server (You can specify server connection info with the `--database-url` option). To build, I think you just need to do a `cabal install`.

## Usage

```
Usage: feeder COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  migrate                  migrate application database
  serve                    serve web application
  import                   import Atom or RSS feed from URL
```
For example to initialize the database,
```
$ feeder migrate
Initializing schema
NOTICE:  relation "schema_migrations" already exists, skipping
Ok:     00_create_item_table.sql
```
Then to import the feed items from a url,
```
$ feeder import https://rss.nytimes.com/services/xml/rss/nyt/World.xml
Imported 63 items from https://rss.nytimes.com/services/xml/rss/nyt/World.xml
```
Then we can serve a simple page which shows all of the feed items
```
$ feeder serve
serving on http://localhost:3000/
```
