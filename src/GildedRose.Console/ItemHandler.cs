namespace GildedRose.Console {
    public abstract class ItemHandler {
        protected readonly Item item;
        
        public static ItemHandler For(Item item) {
            if (ItemType.IsConjured(item)) return new RegularItemHandler(item, -2);
            if (ItemType.IsAgedBrie(item)) return new RegularItemHandler(item, 1);
            if (ItemType.IsSulfuras(item)) return new SulfurasItemHandler(item);
            if (ItemType.IsBackstagePass(item)) return new BackstagePassItemHandler(item);
            return new RegularItemHandler(item, -1);
        }

        public void Update() {
            UpdateQuality();
            if (IsForSale()) item.SellIn -= 1;
            UpdateExpiredQuality();
        }

        protected ItemHandler(Item item) {
            this.item = item;
        }

        protected virtual int MaxQuality() {
            return 50;
        }
        
        protected virtual bool IsForSale() {
            return true;
        }

        protected abstract int QualityChange();
        protected abstract int QualityChangeWhenExpired();

        private void UpdateQuality() {
            ChangeQualityBy(QualityChange());
        }

        private void UpdateExpiredQuality() {
            if (item.SellIn < 0)
                ChangeQualityBy(QualityChangeWhenExpired());
        }

        private void ChangeQualityBy(int value) {
            item.Quality += value;
            if (item.Quality < 0) item.Quality = 0;
            if (item.Quality >= MaxQuality()) item.Quality = MaxQuality();
        }
    }
}