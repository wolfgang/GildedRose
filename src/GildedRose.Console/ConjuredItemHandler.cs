namespace GildedRose.Console {
    public class ConjuredItemHandler : ItemHandler {
        public ConjuredItemHandler(Item item) : base(item, -2) { }

        protected override int QualityChangeWhenExpired() {
            return -4; // Double the degradation rate when expired
        }
    }
}