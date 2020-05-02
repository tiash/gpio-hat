# GPIO-Hat
A hat and software for testing and programming ICs using a raspberry PI.

I recently ordered many 74LSxx series ICs from China and wanted a way to test
the chips to make sure they worked as expected before I put them in my circuits.

This was the result:
**TODO** Add image**

And some ocaml programs for driving the whole thing.

# Hardware
Several people made some really neat [arduino](https://blog.arduino.cc/2018/02/05/automated-ic-testing-with-arduino-mega/) [powered](https://hackaday.com/2018/02/15/building-an-arduino-smart-ic-tester-for-25/) [IC](https://www.electronicsforu.com/electronics-projects/hardware-diy/arduino-based-digital-ic-tester-truth-table) [testers](https://blog.arduino.cc/2018/02/05/automated-ic-testing-with-arduino-mega/), but these all seem to be running into the limits of what the platform could do, so I thought why no use a PI, it has WAY more compute power!

So the idea of a PI Hat ~~with screen~~ and ZIF socket for testing ICs was born.

The Raspberry PI has actually surprisingly few IO pins and they are all on 3.3V, I was looking for something that was a little more robust and could handle 5V TTL levels. With a bit of searching I discovered the [TCA6424A](https://www.ti.com/product/TCA6424A), 24pin Level shifting I2C GPIO expander, a perfect fit (mostly).
The part is a little small though, but nothing that a cheap SMD assembly shop couldn't handle.

The board features a space for a ZIF socket for inserting the IC to be tested, a Jumper to select the drive voltage (3.3V or 5V), and some pin headers for the Voltage and Ground (To hook up an external voltage supply or to use if an IC requires more power than the TCA6424 can supply).
I also included the EEPROM for the HAT-id, and a header for hooking up a small touch screen though I ended up not using either.
**TODO** Image of the PCB with the different features highlighted

You can find the design files and everything I sent to the PCB and assembly shop under [Hardware/].

NB: This was my first ever PCB design (and also my first forray into hardware electronics), so I probably did all the wrong things (feedback welcome!).


# Setup
1. Fabricate the HAT and attach it to a Raspberry PI
   I got mine fabricated and part assembled by JLCPCB and then soldered on the headers and ZIF socket.
   (Soldering the TCA6424A is way beyond my non-existing soldering skill).
2. Mount to a raspberry PI (Hopefully you added some mounting screws before soldering the ZIF socket)
3. Install OPAM and follow the instructions to setup a simple ocaml environment
   ```bash
   sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
   opam init
   opam install dune core
   ```
4. Get the code
   ```bash
   git clone https://github.com/tiash/gpio-hat
   ``` 
5. Build the tools
   ```bash
   dune build --auto-promote
   ```
   
# Usage

## Testing an IC to make it work
1. Make sure you have the voltage jumper set correctly for your ICs (don't want to burn out the IC, or the raspberry pi!)
2. Insert the IC into the socket & pull the leaver to lock the socket in place an make contact.
   Make sure that the IC is in the position furthest from the IO ports, with the Pin 1 mark pointing away
      **TODO** Image of the board with an IC inserted correctly, highlight the pin1 mark
3. Run the IC tester tool
   ```bash
   bin/ic-tester.exe test IC_NAME
   ```
   If you have a bunch of ICs of the same type you want to test you can also run
   ```bash
   bin/ix-tester.exe batch-test IC_NAME
   ```
   And press any key to run the test after swapping the ICs
4. If the IC has defects (or you selected the wrong IC_NAME) the tool will report errors
   and print the combination of inputs that triggered the unexpected output.

## Programming an 16C28 EEPROM
If you're using a different EEPROM you can easily add support to it to the tool.
**TODO** Point to where to code needs to be edited
1. Prepare a binary file with the desired rom content
2. Insert the EEPROM rom in the correct position (furthest from the IO ports with PIN1 furthest away).2. P
3. Run
   ```bash
   bin/flasher.exe 16c28 write FILE
   ```
   This will read the IC, write the changes, and finally read back the ROM to verify the write succeeded.
   Depending on where you sourced your EEPROMs you may find some faulty chips.
   
# Things to maybe do someday

## Implement auto detection of ICs
The testing programs have enough information to allow auto-detecting the IC thats inserted.

## Finish the hardware inputs
I designed the HAT with a headers for a [small touch screen](https://www.banggood.com/1_8-Inch-LCD-Screen-SPI-Serial-Port-Module-TFT-Color-Display-Touch-Screen-ST7735-p-1414465.html), but never actually hooked it up...

It should be possible to have the screen show a simple UI for selecting the IC, running the tests and showing the results.
 
## Cleanup and improve the design of the HAT Board
The board layout should be changed to put the ICs on the under side, or not underneat the ZIF socket.
There's also lots of features that could make this better.
Here's a few items that come to mind:
- Integrated supply voltage controller instead of the 3.3V/5.0V jumper,
- Use GPIO expander in a larger package that's more suited to hand soldering
- Use more sophisticated GPIO drivers,
  Something with a controllable pull-up/pull-down to detect non driven pins.
  Maybe even use an ADC to detect output voltages
- Improve the response time and allow high speed reading of pins. Something to allow measuring the response time of the IC.
