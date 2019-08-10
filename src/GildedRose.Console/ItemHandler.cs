namespace GildedRose.Console {
    public abstract class ItemHandler {
        protected readonly Item item;

        protected ItemHandler(Item item) {
            this.item = item;
        }

        public void Update() {
            UpdateQuality();
            if (IsForSale()) item.SellIn -= 1;
            UpdateExpiredQuality();
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

        protected abstract int QualityChange();
        protected abstract int QualityChangeWhenExpired();

        protected virtual int MaxQuality() {
            return 50;
        }

        protected virtual bool IsForSale() {
            return true;
        }
    }
}