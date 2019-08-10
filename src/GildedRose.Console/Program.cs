using System.Collections.Generic;

namespace GildedRose.Console {
    public class Program {
        private IList<Item> Items;
        private List<ItemHandler> ItemHandlers;
        private IWriter writer;

        private static void Main(string[] args) {
            var app = Default(new ConsoleWriter());
            for (int day = 0; day < 30; ++day) {
                app.UpdateQuality();
                app.DumpItems(day);
            }
        }

        private Program(IWriter writer) {
            this.writer = writer;
            ItemHandlers = new List<ItemHandler>();
            Items = new List<Item>{
                new Item{Name = "+5 Dexterity Vest", SellIn = 10, Quality = 20},
                new Item{Name = "Aged Brie", SellIn = 2, Quality = 0},
                new Item{Name = "Elixir of the Mongoose", SellIn = 5, Quality = 7},
                new Item{Name = "Sulfuras, Hand of Ragnaros", SellIn = 0, Quality = 80},
                new Item{
                    Name = "Backstage passes to a TAFKAL80ETC concert",
                    SellIn = 15,
                    Quality = 20
                },
                new Item{Name = "Conjured Mana Cake", SellIn = 3, Quality = 6}
            };

            foreach (var item in Items) {
                ItemHandlers.Add(ItemHandlerFor(item));
            }
        }

        private static ItemHandler ItemHandlerFor(Item item) {
            if (ItemType.IsConjured(item)) return ItemHandler.Conjured(item);
            if (ItemType.IsAgedBrie(item)) return ItemHandler.AgedBrie(item);
            if (ItemType.IsSulfuras(item)) return ItemHandler.Sulfuras(item);
            if (ItemType.IsBackstagePass(item)) return ItemHandler.BackstagePass(item);
            return ItemHandler.Default(item);
        }

        public void UpdateQuality() {
            foreach (var handler in ItemHandlers) {
                handler.Update();
            }
        }

        public static Program Default(IWriter writer) {
            return new Program(writer);
        }

        public void DumpItems(int day) {
            writer.WriteLine($"--- day {day} ---");
            writer.WriteLine("Name, Quality, SellIn");
            foreach (var item in Items) {
                writer.WriteLine($"{item.Name}, {item.Quality}, {item.SellIn}");
            }
        }
    }

    public class Item {
        public string Name { get; set; }

        public int SellIn { get; set; }

        public int Quality { get; set; }
    }

    internal class ConsoleWriter : IWriter {
        public void WriteLine(string line) {
            System.Console.WriteLine(line);
        }
    }
}