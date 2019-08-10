namespace GildedRose.Console {
    public abstract class ItemHandler {
        private const int MIN_QUALITY = 0;
        private const int MAX_QUALITY = 50;

        public readonly Item item;

        protected ItemHandler(Item item) {
            this.item = item;
        }

        public  void ChangeQualityBy(int value) {
            item.Quality += value;
            if (item.Quality < 0) item.Quality = 0;
            if (!ItemType.IsSulfuras(item) && item.Quality >= 50) item.Quality = 50;
        }

        public abstract int QualityChange();

            public int QualityChangeForExpired() {
            if (ItemType.IsBackstagePass(item)) return -item.Quality;
            if (ItemType.IsAgedBrie(item)) return 1;
            return GetQualityDecrease();
        }

        private bool DecreasesInQuality() {
            return !ItemType.IsBackstagePass(item);

        }

        private int GetQualityDecrease() {
            return -1;
        }

        private  int GetQualityIncrease() {
            if (!ItemType.IsBackstagePass(item) || item.SellIn >= 11) return 1;
            return item.SellIn < 6 ? 3 : 2;
        }
    }
}