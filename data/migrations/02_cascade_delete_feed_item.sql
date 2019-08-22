alter table feed_item drop constraint feed_item_feed_fkey;

alter table feed_item add constraint feed_item_feed_fkey foreign key (feed) references feed (link) on delete cascade;
