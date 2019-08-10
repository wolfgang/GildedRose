namespace GildedRose.Console {
    public class DefaultItemHandler : ItemHandler {
        public DefaultItemHandler(Item item) : base(item) { }

        public override int QualityChange() {
            return -1;
        }
    }
}