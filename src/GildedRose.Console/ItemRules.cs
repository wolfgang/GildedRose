using System.Runtime.InteropServices.WindowsRuntime;

namespace GildedRose.Console {
    internal static class ItemRules {
        private const int MIN_QUALITY = 0;
        private const int MAX_QUALITY = 50; 
            
        
        public static int GetPreSaleQualityChange(Item item) {
            if (decreasesInQuality(item)) return -1;
            return GetQualityIncrease(item);
        }

        private static bool decreasesInQuality(Item item) {
            return !isAgedBrie(item) &&
                   !isBackstagePass(item) &&
                   !isSulfuras(item) &&
                   item.Quality > MIN_QUALITY;
        }

        private static int GetQualityIncrease(Item item) {
            var increase = 0;
            if (item.Quality < MAX_QUALITY) {
                increase += 1;

                if (isBackstagePass(item)) {
                    if (item.SellIn < 11) {
                        increase += 1;
                    }

                    if (item.SellIn < 6) {
                        increase += 1;
                    }
                }
            }

            return increase;
        }

        public static int GetPostSaleQualityChange(Item item) {
            if (isBackstagePass(item)) return -item.Quality;
            if (item.Quality == MIN_QUALITY || item.Quality >= MAX_QUALITY) return 0;
            if (isAgedBrie(item)) return 1;
            return -1;
        }

        private static bool isAgedBrie(Item item) {
            return item.Name == "Aged Brie";
        }

        private static bool isBackstagePass(Item item) {
            return item.Name.StartsWith("Backstage passes");
        }

        public static bool isSulfuras(Item item) {
            return item.Name.StartsWith("Sulfuras");
        }
    }
}