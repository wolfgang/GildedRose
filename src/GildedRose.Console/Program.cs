using System.Collections.Generic;

namespace GildedRose.Console {
    public class Program {
        private IList<Item> Items;
        private IWriter writer;

        private static void Main(string[] args) {
            var app = Default(new ConsoleWriter());
            for (int day = 0; day < 30; ++day) {
                app.UpdateQuality();
                app.DumpItems(day);
            }
        }

        public void UpdateQuality() {
            foreach (var item in Items) {
                ItemRules.ChangeQualityBy(ItemRules.GetPreSaleQualityChange(item), item);
                
                if (!ItemRules.isSulfuras(item)) {
                    item.SellIn -= 1;
                }

                if (item.SellIn < 0) {
                    ItemRules.ChangeQualityBy(ItemRules.GetPostSaleQualityChange(item), item);
                }
            }
        }

        public static Program Default(IWriter writer) {
            return new Program(){
                writer = writer,
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
                }
            };
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