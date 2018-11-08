# WNV_Mechanistic_Model
Code for: Kain and Bolker 2019: Predicting West Nile virus transmission in North American bird communities using phylogenetic mixed effects models and eBird citizen science data. 

All data needed to run these models can be obtained in the supplementary material of Kain and Bolker 2019 at:   
URL HERE upon publication

This repo is designed to be used with Kain and Bolker 2019. 

Empty folders are one of three types: 
-- need to have data placed in them prior to running the code. Data available as the supplementary material to Kain and Bolker 2019 at: URL HERE is organized in the appropriate folders
        -- trees (phylogenetic tree)
        -- ebird_zip_fresh (the .zip ebird file)
        -- data (all other data)
-- model components
        -- stan (stan model definitions)
-- start empty but get filled as part of the automated workflow when output is saved to disk
    -- ebird_data_dump
    -- ebird_data_for_R
    -- ebird_pieces
    -- ebird_unzip
    -- ebird_zip_dump
    -- saved_fits
    -- saved_matching
    -- saved_model_results
    -- saved_output
    
