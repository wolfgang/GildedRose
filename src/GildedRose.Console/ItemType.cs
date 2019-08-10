namespace GildedRose.Console {
    public class ItemType {
        public static  bool IsAgedBrie(Item item) {
            return item.Name == "Aged Brie";
        }

        public static bool IsBackstagePass(Item item) {
            return item.Name.StartsWith("Backstage passes");
        }

        public static bool IsSulfuras(Item item) {
            return item.Name.StartsWith("Sulfuras");
        }

        public static bool IsConjured(Item item) {
            return item.Name.StartsWith("Conjured");
        }

    }
}