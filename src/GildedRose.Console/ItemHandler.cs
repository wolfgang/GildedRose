namespace GildedRose.Console {
    public class ItemHandler {
        private const int MIN_QUALITY = 0;
        private const int MAX_QUALITY = 50;

        public Item item;

        public ItemHandler(Item item) {
            this.item = item;
        }


        public  void ChangeQualityBy(int value) {
            this.item.Quality += value;
            if (this.item.Quality < 0) this.item.Quality = 0;
        }

        public int QualityChange() {
            if (DecreasesInQuality()) return GetQualityDecrease();
            return GetQualityIncrease();
        }

        public int QualityChangeForExpired() {
            if (IsBackstagePass()) return -this.item.Quality;
            if (this.item.Quality == MIN_QUALITY || this.item.Quality >= MAX_QUALITY) return 0;
            if (IsAgedBrie()) return 1;
            return GetQualityDecrease();
        }

        private bool DecreasesInQuality() {
            return !IsAgedBrie() &&
                   !IsBackstagePass() &&
                   !IsSulfuras() &&
                   item.Quality > MIN_QUALITY;
        }

        private int GetQualityDecrease() {
            if (IsConjured()) return -2;
            return -1;
        }

        private  int GetQualityIncrease() {
            if (item.Quality >= MAX_QUALITY) return 0;
            if (!IsBackstagePass() || item.SellIn >= 11) return 1;
            return item.SellIn < 6 ? 3 : 2;
        }

        private  bool IsAgedBrie() {
            return item.Name == "Aged Brie";
        }

        private bool IsBackstagePass() {
            return item.Name.StartsWith("Backstage passes");
        }

        public bool IsSulfuras() {
            return item.Name.StartsWith("Sulfuras");
        }

        private bool IsConjured() {
            return item.Name.StartsWith("Conjured");
        }
    }
}