namespace GildedRose.Console {
    public class ConjuredItemHandler : ItemHandler {
        public ConjuredItemHandler(Item item) : base(item) { }

        public override int QualityChange() {
            return -2;
        }

        public override int QualityChangeForExpired() {
            return QualityChange();
        }
    }
    
}