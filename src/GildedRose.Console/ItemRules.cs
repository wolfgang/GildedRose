namespace GildedRose.Console {
    internal static class ItemRules {
        public static bool decreasesInQuality(Item item) {
            return !isAgedBrie(item) &&
                   !isBackstagePass(item) &&
                   !isSulfuras(item) &&
                   item.Quality > 0;
        }

        public static void IncreaseQuality(Item item) {
            if (item.Quality < 50) {
                item.Quality += 1;
            }
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