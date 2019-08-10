namespace GildedRose.Console {
    public class BackstagePassItemHandler : ItemHandler{
        public BackstagePassItemHandler(Item item) : base(item) { }

        protected override int QualityChange() {
            if (item.SellIn >= 11) return 1;
            return item.SellIn < 6 ? 3 : 2;
        }

        protected override int QualityChangeWhenExpired() {
            return -item.Quality;
        }
    }
}