namespace GildedRose.Console {
    internal static class ItemRules {
        public static bool decreasesInQuality(Item item) {
            return item.Name != "Aged Brie" &&
                   !item.Name.StartsWith("Backstage passes") &&
                   !item.Name.StartsWith("Sulfuras") &&
                   item.Quality > 0;
        }
    }
}