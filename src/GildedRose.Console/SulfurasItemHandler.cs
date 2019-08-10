namespace GildedRose.Console {
    public class SulfurasItemHandler :ItemHandler{
        public SulfurasItemHandler(Item item) : base(item) { }

        public override int QualityChange() {
            return 0;
        }
    }
}