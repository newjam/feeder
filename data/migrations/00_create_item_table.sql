create table feed_item (
  guid text primary key,
  title text not null,
  link text not null,
  date timestamp with time zone not null
)
