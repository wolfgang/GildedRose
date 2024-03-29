namespace GildedRose.Console {
    public class SulfurasItemHandler : ItemHandler {
        public SulfurasItemHandler(Item item) : base(item, 0) { }

        protected override int MaxQuality() {
            return 80;
        }

        protected override bool IsForSale() {
            return false;
        }
    }
}