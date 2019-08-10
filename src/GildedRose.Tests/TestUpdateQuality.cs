using System.Collections.Generic;
using GildedRose.Console;
using NUnit.Framework;

namespace GildedRose.Tests {
    public class TestUpdateQuality {
        [Test]
        public void UpdateQualityFor30Days() {
            var writer = new ToStringsWriter();
            var app = Program.Default(writer);

            for (int day = 0; day < 30; ++day) {
                app.UpdateQuality();
                app.DumpItems(day);
                
            } 

            writer.AssertLines(new List<string>{
                "--- day 0 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 19, 9",
                "Aged Brie, 1, 1",
                "Elixir of the Mongoose, 6, 4",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 21, 14",
                "Conjured Mana Cake, 4, 2",
                "--- day 1 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 18, 8",
                "Aged Brie, 2, 0",
                "Elixir of the Mongoose, 5, 3",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 22, 13",
                "Conjured Mana Cake, 2, 1",
                "--- day 2 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 17, 7",
                "Aged Brie, 4, -1",
                "Elixir of the Mongoose, 4, 2",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 23, 12",
                "Conjured Mana Cake, 0, 0",
                "--- day 3 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 16, 6",
                "Aged Brie, 6, -2",
                "Elixir of the Mongoose, 3, 1",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 24, 11",
                "Conjured Mana Cake, 0, -1",
                "--- day 4 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 15, 5",
                "Aged Brie, 8, -3",
                "Elixir of the Mongoose, 2, 0",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 25, 10",
                "Conjured Mana Cake, 0, -2",
                "--- day 5 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 14, 4",
                "Aged Brie, 10, -4",
                "Elixir of the Mongoose, 0, -1",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 27, 9",
                "Conjured Mana Cake, 0, -3",
                "--- day 6 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 13, 3",
                "Aged Brie, 12, -5",
                "Elixir of the Mongoose, 0, -2",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 29, 8",
                "Conjured Mana Cake, 0, -4",
                "--- day 7 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 12, 2",
                "Aged Brie, 14, -6",
                "Elixir of the Mongoose, 0, -3",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 31, 7",
                "Conjured Mana Cake, 0, -5",
                "--- day 8 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 11, 1",
                "Aged Brie, 16, -7",
                "Elixir of the Mongoose, 0, -4",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 33, 6",
                "Conjured Mana Cake, 0, -6",
                "--- day 9 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 10, 0",
                "Aged Brie, 18, -8",
                "Elixir of the Mongoose, 0, -5",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 35, 5",
                "Conjured Mana Cake, 0, -7",
                "--- day 10 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 8, -1",
                "Aged Brie, 20, -9",
                "Elixir of the Mongoose, 0, -6",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 38, 4",
                "Conjured Mana Cake, 0, -8",
                "--- day 11 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 6, -2",
                "Aged Brie, 22, -10",
                "Elixir of the Mongoose, 0, -7",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 41, 3",
                "Conjured Mana Cake, 0, -9",
                "--- day 12 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 4, -3",
                "Aged Brie, 24, -11",
                "Elixir of the Mongoose, 0, -8",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 44, 2",
                "Conjured Mana Cake, 0, -10",
                "--- day 13 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 2, -4",
                "Aged Brie, 26, -12",
                "Elixir of the Mongoose, 0, -9",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 47, 1",
                "Conjured Mana Cake, 0, -11",
                "--- day 14 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -5",
                "Aged Brie, 28, -13",
                "Elixir of the Mongoose, 0, -10",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 50, 0",
                "Conjured Mana Cake, 0, -12",
                "--- day 15 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -6",
                "Aged Brie, 30, -14",
                "Elixir of the Mongoose, 0, -11",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -1",
                "Conjured Mana Cake, 0, -13",
                "--- day 16 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -7",
                "Aged Brie, 32, -15",
                "Elixir of the Mongoose, 0, -12",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -2",
                "Conjured Mana Cake, 0, -14",
                "--- day 17 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -8",
                "Aged Brie, 34, -16",
                "Elixir of the Mongoose, 0, -13",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -3",
                "Conjured Mana Cake, 0, -15",
                "--- day 18 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -9",
                "Aged Brie, 36, -17",
                "Elixir of the Mongoose, 0, -14",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -4",
                "Conjured Mana Cake, 0, -16",
                "--- day 19 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -10",
                "Aged Brie, 38, -18",
                "Elixir of the Mongoose, 0, -15",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -5",
                "Conjured Mana Cake, 0, -17",
                "--- day 20 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -11",
                "Aged Brie, 40, -19",
                "Elixir of the Mongoose, 0, -16",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -6",
                "Conjured Mana Cake, 0, -18",
                "--- day 21 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -12",
                "Aged Brie, 42, -20",
                "Elixir of the Mongoose, 0, -17",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -7",
                "Conjured Mana Cake, 0, -19",
                "--- day 22 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -13",
                "Aged Brie, 44, -21",
                "Elixir of the Mongoose, 0, -18",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -8",
                "Conjured Mana Cake, 0, -20",
                "--- day 23 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -14",
                "Aged Brie, 46, -22",
                "Elixir of the Mongoose, 0, -19",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -9",
                "Conjured Mana Cake, 0, -21",
                "--- day 24 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -15",
                "Aged Brie, 48, -23",
                "Elixir of the Mongoose, 0, -20",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -10",
                "Conjured Mana Cake, 0, -22",
                "--- day 25 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -16",
                "Aged Brie, 50, -24",
                "Elixir of the Mongoose, 0, -21",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -11",
                "Conjured Mana Cake, 0, -23",
                "--- day 26 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -17",
                "Aged Brie, 50, -25",
                "Elixir of the Mongoose, 0, -22",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -12",
                "Conjured Mana Cake, 0, -24",
                "--- day 27 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -18",
                "Aged Brie, 50, -26",
                "Elixir of the Mongoose, 0, -23",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -13",
                "Conjured Mana Cake, 0, -25",
                "--- day 28 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -19",
                "Aged Brie, 50, -27",
                "Elixir of the Mongoose, 0, -24",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -14",
                "Conjured Mana Cake, 0, -26",
                "--- day 29 ---",
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 0, -20",
                "Aged Brie, 50, -28",
                "Elixir of the Mongoose, 0, -25",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 0, -15",
                "Conjured Mana Cake, 0, -27",
            });
        }

        private class ToStringsWriter : IWriter {
            private readonly List<string> lines = new List<string>();

            public void WriteLine(string line) {
                lines.Add(line);
            }

            public void AssertLines(List<string> expectedLines) {
                Assert.AreEqual(expectedLines, lines);
            }
        }
    }
}