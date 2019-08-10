namespace GildedRose.Console {
    public class SulfurasItemHandler :ItemHandler{
        public SulfurasItemHandler(Item item) : base(item) { }

        protected override int QualityChange() {
            return 0;
        }

        protected override int QualityChangeWhenExpired() {
            return QualityChange();
        }

        protected override int MaxQuality() {
            return 80;
        }

        protected override bool IsForSale() {
            return false;
        }
    }
}