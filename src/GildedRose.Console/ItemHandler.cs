namespace GildedRose.Console {
    public abstract class ItemHandler {
        public readonly Item item;

        protected ItemHandler(Item item) {
            this.item = item;
        }

        public void ChangeQualityBy(int value) {
            item.Quality += value;
            if (item.Quality < 0) item.Quality = 0;
            if (!ItemType.IsSulfuras(item) && item.Quality >= 50) item.Quality = 50;
        }

        public abstract int QualityChange();
        public abstract int QualityChangeForExpired();
    }
}