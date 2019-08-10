namespace GildedRose.Console {
    public class DefaultItemHandler : ItemHandler {
        public DefaultItemHandler(Item item) : base(item) { }

        protected override int QualityChange() {
            return -1;
        }

        protected override int QualityChangeWhenExpired() {
            return QualityChange();
        }
    }
}