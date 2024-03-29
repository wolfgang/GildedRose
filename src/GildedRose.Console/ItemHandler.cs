namespace GildedRose.Console {
    public class ItemHandler {
        protected readonly Item item;

        private readonly int qualityChange;
        
        public static ItemHandler For(Item item) {
            if (ItemType.IsConjured(item)) return new ItemHandler(item, -2);
            if (ItemType.IsAgedBrie(item)) return new ItemHandler(item, 1);
            if (ItemType.IsSulfuras(item)) return new SulfurasItemHandler(item);
            if (ItemType.IsBackstagePass(item)) return new BackstagePassItemHandler(item);
            return new ItemHandler(item, -1);
        }

        public void Update() {
            UpdateQuality();
            if (IsForSale()) item.SellIn -= 1;
            UpdateExpiredQuality();
        }

        protected ItemHandler(Item item, int qualityChange) {
            this.item = item;
            this.qualityChange = qualityChange;
        }

        protected virtual int MaxQuality() {
            return 50;
        }
        
        protected virtual bool IsForSale() {
            return true;
        }

        protected virtual int QualityChange() {
            return qualityChange;
        }

        protected virtual int QualityChangeWhenExpired() {
            return qualityChange;
        }

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