# Machine learning to estimate mimetic accuracy


This directory contains scripts and data to use machine learning to
calculate the static visual mimetic accuracy of a variety of ant
mimics, based on their photos. Note that unlike the other methods in
this project, this method directory is not entirely self-contained. It
uses photos from the `HumanPredators` method.

It works by requesting Google cloud vision to identify the contents of
photographs of the mimics. Google cloud vision assigns a set of labels
to each photograph (such as `Insect`, `Organism`, `Macro photography`,
`Arachnid` and so on), together with a probability that the photograph
depicts that label. For our purposes, we use the probability that the
photo depicts some kind of ant as the mimetic accuracy score.

Note that Google cloud vision (https://cloud.google.com/vision/) is a
commercial service. At the time of writing, it is free to sign up to,
and it allows a number of free transactions per month.

You must have both Ruby (https://www.ruby-lang.org) and R
(https://cran.r-project.org/) installed to run these scripts.

## Workflow

### Step 1

Run `GVaccuracy.bat` (on Windows, or `GVaccuracy.sh` on *nix) to read the input CSV
file (`data/mimics.csv`) to obtain a list of photo files, query Google cloud vision, and create an output
CSV (`output/gv_raw_labels.csv`) with the returned labels for each
image. This is the step that costs money, although if you add new
images to `data/mimics.csv` and re-run `GVaccuracy.bat`, it will only
process the new images. All labels and probabilities assigned by
Google cloud vision to an image are written to `output/gv_raw_labels.csv`.

`GVaccuracy.bat` is a minimal BAT file that defines some configuration
properties and runs a Ruby script, `GVaccuracy.rb`.  `GVaccuracy.bat`
assumes that the photo files are in the folder `../Human
trials/EatUp/`. It also specifies the Google vision account
credentials to be used. You may need to work through the Google
how-to-guides to properly set up and specify your account. You will
need to edit `GVaccuracy.bat` accordingly.

### Step 2

Next run the R script `extract-accuracy.R` to read the raw labels from
`gv_raw_labels.csv` and interpret them to create a CSV file with
mimetic accuracy per image. This script implements an algorithm that
is specific to our purpose: it extracts the highest probability label
per image that ends with the word `ant`. That probability is used as
the mimetic accuracy score. If there is no such label for an image, an
accuracy of 0 is assigned. Results are written to `output/Google
vision-accuracy-images.csv`. Results are averaged per species/angle
combination and per species (and written to CSV files), to obtain
species accuracy scores. All output CSV files are copied to the
`../output` directory.

## Input

- `data/mimics.csv` was copied from `../Human trials/EatUp/photo_info.csv`
           and all non-mimics were manually removed (apart from a
           couple of ants that were retained as a sanity check).

## Output
- `output/gv_raw_labels.csv` - output CSV file, with a row per image and label
- `output/Machine learning-accuracy-images.csv` - output file with row
                                         per image, columns are image
                                         URL and how ant-like the
                                         image is
- `output/Machine learning-accuracy-species-angles.csv` - output file
    with row per species and angle, columns are taxonomic details,
    whether the species is considered a mimic, the angle of the photo,
    mimetic accuracy, and the number of photos that were averaged to
    derive the accuracy    
- `output/Machine learning-accuracy-species.csv` - output file with row
    per species, columns are taxonomic details, whether the species is
    considered a mimic, mimetic accuracy, and the number of photos
    used to derive the accuracy
    

