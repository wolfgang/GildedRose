namespace GildedRose.Console {
    public class RegularItemHandler : ItemHandler
    {
        private readonly int qualityChange;

        protected RegularItemHandler(Item item, int qualityChange) : base(item) {
            this.qualityChange = qualityChange;
        }
        protected override int QualityChange() {
            return qualityChange;
        }

        protected override int QualityChangeWhenExpired() {
            return qualityChange;
        }
    }
}