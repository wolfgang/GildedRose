using System;

namespace GildedRose.Console {
    public static class ItemType {
        public static bool IsAgedBrie(Item item) {
            return item?.Name?.Equals("Aged Brie", StringComparison.OrdinalIgnoreCase) == true;
        }

        public static bool IsBackstagePass(Item item) {
            return item?.Name?.StartsWith("Backstage passes", StringComparison.OrdinalIgnoreCase) == true;
        }

        public static bool IsSulfuras(Item item) {
            return item?.Name?.StartsWith("Sulfuras", StringComparison.OrdinalIgnoreCase) == true;
        }

        public static bool IsConjured(Item item) {
            return item?.Name?.StartsWith("Conjured", StringComparison.OrdinalIgnoreCase) == true;
        }
    }
}