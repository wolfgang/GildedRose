using System.Collections.Generic;
using GildedRose.Console;
using NUnit.Framework;

namespace GildedRose.Tests {
    public class TestUpdateQuality {
        [Test]
        public void ItemsAfterOnDay0() {
            var writer = new ToStringsWriter();
            var program = Program.Default(writer);
            
            program.UpdateQuality();
            program.dumpItems();

            writer.AssertLines(new List<string>{
                "Name, Quality, SellIn",
                "+5 Dexterity Vest, 19, 9",
                "Aged Brie, 1, 1",
                "Elixir of the Mongoose, 6, 4",
                "Sulfuras, Hand of Ragnaros, 80, 0",
                "Backstage passes to a TAFKAL80ETC concert, 21, 14",
                "Conjured Mana Cake, 5, 2"
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
