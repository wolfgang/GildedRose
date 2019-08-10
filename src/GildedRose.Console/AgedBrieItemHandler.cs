namespace GildedRose.Console {
    public class AgedBrieItemHandler : ItemHandler{
        public AgedBrieItemHandler(Item item) : base(item) { }

        public override int QualityChange() {
            return 1;
        }

        public override int QualityChangeForExpired() {
            return QualityChange();
        }
    }
}