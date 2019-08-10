using System.Runtime.InteropServices.WindowsRuntime;

namespace GildedRose.Console {
    internal static class ItemRules {
        private const int MIN_QUALITY = 0;
        private const int MAX_QUALITY = 50;


        public static void ChangeQualityBy(int value, Item item) {
            item.Quality += value;
            if (item.Quality < 0) item.Quality = 0;
        }

        public static int QualityChangeFor(Item item) {
            if (DecreasesInQuality(item)) return GetQualityDecrease(item);
            return GetQualityIncrease(item);
        }

        public static int QualityChangeForExpired(Item item) {
            if (IsBackstagePass(item)) return -item.Quality;
            if (item.Quality == MIN_QUALITY || item.Quality >= MAX_QUALITY) return 0;
            if (IsAgedBrie(item)) return 1;
            return GetQualityDecrease(item);
        }

        private static bool DecreasesInQuality(Item item) {
            return !IsAgedBrie(item) &&
                   !IsBackstagePass(item) &&
                   !IsSulfuras(item) &&
                   item.Quality > MIN_QUALITY;
        }

        private static int GetQualityDecrease(Item item) {
            if (IsConjured(item)) return -2;
            return -1;
        }

        private static int GetQualityIncrease(Item item) {
            if (item.Quality >= MAX_QUALITY) return 0;
            if (!IsBackstagePass(item) || item.SellIn >= 11) return 1;
            return item.SellIn < 6 ? 3 : 2;
        }

        private static bool IsAgedBrie(Item item) {
            return item.Name == "Aged Brie";
        }

        private static bool IsBackstagePass(Item item) {
            return item.Name.StartsWith("Backstage passes");
        }

        public static bool IsSulfuras(Item item) {
            return item.Name.StartsWith("Sulfuras");
        }

        private static bool IsConjured(Item item) {
            return item.Name.StartsWith("Conjured");
        }
    }
}