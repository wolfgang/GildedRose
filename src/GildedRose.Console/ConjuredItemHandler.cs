namespace GildedRose.Console {
    public class ConjuredItemHandler : ItemHandler {
        public ConjuredItemHandler(Item item) : base(item) { }

        protected override int QualityChange() {
            return -2;
        }

        protected override int QualityChangeWhenExpired() {
            return QualityChange();
        }
    }
    
}