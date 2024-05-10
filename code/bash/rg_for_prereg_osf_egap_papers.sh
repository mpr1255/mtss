#This is the series of commands I used to pull all the osf.io links and egap pre-registration ids from the file all_osf_regs.json, then search them across all the files in the data/txt folder. 

# pull all the osf.io links and egap IDs
#(this is from within the folder /output)
rg 'osf.*?"|E[0-9]{4}.*?"' -o all_osf_regs.json | tr -d '"' | sort -u > osf_links_and_reg_ids_in_all_osf_regs_json.txt

# search them across all files and output them
#(this is from within the folder /data/txt)
rg -o `cat ../../output/osf_links_and_reg_ids_in_all_osf_regs_json.txt | paste -sd "|"` > ../../output/matches_for_osf_links_and_reg_ids.txt