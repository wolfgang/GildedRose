namespace GildedRose.Console {
    internal static class ItemRules {
        public static bool decreasesInQuality(Item item) {
            return !isAgedBrie(item) &&
                   !isBackstagePass(item) &&
                   !isSulfuras(item) &&
                   item.Quality > 0;
        }

        public static int GetQualityIncrease(Item item) {
            var increase = 0;
            if (item.Quality < 50) {
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
            if (item.Quality == 0) return 0;
            if (isAgedBrie(item) && item.Quality < 50) return 1;
            
            int change = 0;
            if (!isAgedBrie(item)) {
                if (!isBackstagePass(item) && !isSulfuras(item)) {
                    change = -1;
                }
                else {
                    change = -item.Quality;
                }
            }

            return change;
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