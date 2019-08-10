namespace GildedRose.Console {
    public class ItemHandler {
        private const int MIN_QUALITY = 0;
        private const int MAX_QUALITY = 50;

        public readonly Item item;

        public ItemHandler(Item item) {
            this.item = item;
        }


        public  void ChangeQualityBy(int value) {
            item.Quality += value;
            if (item.Quality < 0) item.Quality = 0;
            if (!IsSulfuras() && item.Quality >= 50) item.Quality = 50;
        }

        public int QualityChange() {
            if (DecreasesInQuality()) return GetQualityDecrease();
            return GetQualityIncrease();
        }

        public int QualityChangeForExpired() {
            if (IsBackstagePass()) return -item.Quality;
            if (IsAgedBrie()) return 1;
            return GetQualityDecrease();
        }

        private bool DecreasesInQuality() {
            return !IsAgedBrie() &&
                   !IsBackstagePass() &&
                   !IsSulfuras();
        }

        private int GetQualityDecrease() {
            if (IsConjured()) return -2;
            return -1;
        }

        private  int GetQualityIncrease() {
            if (IsSulfuras()) return 0;
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