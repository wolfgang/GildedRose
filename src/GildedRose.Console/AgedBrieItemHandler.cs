namespace GildedRose.Console {
    public class AgedBrieItemHandler : ItemHandler{
        public AgedBrieItemHandler(Item item) : base(item) { }

        protected override int QualityChange() {
            return 1;
        }

        protected override int QualityChangeWhenExpired() {
            return QualityChange();
        }
    }
}