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
            int change = 0;
            if (!isAgedBrie(item)) {
                if (item.Quality > 0 && !ItemRules.isBackstagePass(item) &&  !ItemRules.isSulfuras(item)) {
                    change = -1;
                }
                else {
                    change = -item.Quality;
                }
            }
            else if (item.Quality < 50) {
                change = 1;
            }

            return change;

        }

        public static bool isAgedBrie(Item item) {
            return item.Name == "Aged Brie";
        }

        public static bool isBackstagePass(Item item) {
            return item.Name.StartsWith("Backstage passes");
        }

        public static bool isSulfuras(Item item) {
            return item.Name.StartsWith("Sulfuras");
        }
    }
}