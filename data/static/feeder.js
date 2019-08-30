let items = document.getElementsByClassName("feed-items__item-date");
for(let item of items) {
	let datetime = luxon.DateTime.fromSQL(item.textContent);
	let formatted = datetime.toLocaleString(luxon.DateTime.DATETIME_MED);
	item.textContent = formatted;
}
