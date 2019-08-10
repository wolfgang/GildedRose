namespace GildedRose.Console {
    public class SulfurasItemHandler :ItemHandler{
        public SulfurasItemHandler(Item item) : base(item) { }

        public override int QualityChange() {
            return 0;
        }

        public override int QualityChangeForExpired() {
            return 0;
        }

        protected override int MaxQuality() {
            return 80;
        }
    }
}