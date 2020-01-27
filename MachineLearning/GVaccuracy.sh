#!sh
export GOOGLE_APPLICATION_CREDENTIALS=Ant mimics-b4c251660912.json
./GVaccuracy.rb --url=webUrl --inID=webUrl --base='../Human trials/EatUp/' --append output/gv_raw_labels.csv data/mimics.csv
