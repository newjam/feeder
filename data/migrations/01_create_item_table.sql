create table feed_item (
  feed text not null references feed (link),
  guid text primary key,
  title text not null,
  link text not null,
  date timestamp with time zone not null
)
