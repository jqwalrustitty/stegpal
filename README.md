# stegpal

## Source

The code attached to the article "19.08 Steganography in .ICO File" that
appeared in [POC||GTFO 19](https://www.alchemistowl.org/pocorgtfo/).  For more
information about the project itself, please read the whole of that fine journal
and raise a glass to *the Reverend Pastor Manul Laphraoig and his flock*.

The source code is also contained in the PDF document itself, as well as the
[favicon](https://www.alchemistowl.org/favicon.ico)

```Shell
$ unzip pocorgtfo19.pdf stegpal-0.2.8.0.tar.gz
$ stegpal extract -o stegpal-0.2.8.0.tar favicon.ico
```

## Building

The code is written in Haskell, and was originally written to use `cabal`
builds, which has a handy sandbox mode.

```Shell
$ cd stegpal
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal install
```

Which should build you: `./.cabal-sandbox/bin/stegpal`


## Using

There's a long and possibly unhelpful `--help` option.  All of the actions are
given a `COMMAND` and generally a `FILE`.  It will output on stdout, so use the
`-o` option for the extract options, to avoid a messy screen.

```
$ stegpal --help
stegpal 0.2.8
stegpal [COMMAND] ... [OPTIONS]
  Bitmap and Icon palette steganography

Common flags:
  -o --outfile=FILE           output file
  -v --verbose                Verbose
  -? --help                   Display help message
  -V --version                Print version information
     --numeric-version        Print just the version number

stegpal inject [OPTIONS] FILE
  Injection using palette histogram

  -p --payload=FILE           injected payload (use "-" for STDIN)
  -k --key=KEY                password for AES128

stegpal extract [OPTIONS] FILE
  Extraction using palette histogram

  -k --key=KEY                password for AES128

stegpal alpha [OPTIONS] FILE
  Alpha channel injection/extraction
  with payload, it tries to inject; without payload, it tries to extract

  -p --payload=FILE           injected payload (use "-" for STDIN)
  -k --key=KEY                password for AES128

stegpal analyse [OPTIONS] FILE
  Various analysis options
  with no options, defaults to all options

  -b --blocksize=INT --block  block size (default: 32)
  -H --hist                   show histogram
  -s --space                  show palette space
  -e --ent                    show entropy
  -p --showpal                create bmp palette
  -a --alpha --alphach        show alpha space
```

## For Example

Injection and extraction can be done with or without a key/password. By default
a `-steg` stem is added to the original file when a payload is injected.
The `-o` option will redirect the output for both injection and extraction to a
file of your choice.

```
$ stegpal inject -v --key qweasd --payload lorem_ipsum.txt sample-16x16.bmp 
File:     sample-16x16.bmp
outFile:  sample-16x16-steg.bmp
payload:  lorem_ipsum.txt
keyStr:   ******
compress: 288/446
imgType:  ImgBMP
space:    714

$ stegpal extract -v -k qweasd sample-16x16-steg.bmp 
File:     sample-16x16-steg.bmp
outFile:  <stdout>
keyStr:   ******
imgType:  ImgBMP
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint oc

```

The `alpha` channel command uses the observation that the fourth byte of the
`RGBQUAD` structure "is reserved and must be zero" is just assumed to be the
case with most image renderers, so it can also be used to pack in a few extra
bytes.

The `analysie` command will display a bunch of data about the image.  The `-p`
option will create the renderings of the palette as a block image (as seen in
the article), and the `-b` option gives you some control over the size of the
resultant file.  It's a BMP, of course.


------
