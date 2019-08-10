namespace GildedRose.Console {
    public class BackstagePassItemHandler : ItemHandler{
        public BackstagePassItemHandler(Item item) : base(item) { }
        public override int QualityChange() {
            if (item.SellIn >= 11) return 1;
            return item.SellIn < 6 ? 3 : 2;
        }

        public override int QualityChangeForExpired() {
            return -item.Quality;
        }
    }
}