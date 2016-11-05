####################################################################
#                        - DATA EXPLORATION -                         
#
# The aim of this script is to take a first look to our data and 
# to do preprocessing, feature extraction/selection, clustering
# and visualitzation.
#
# Authors: Lluc Bov? and Aleix Trasserra
# Autumn 2016
####################################################################

####################################################################
# PREPROCESSING 
####################################################################

data <- read.csv("data/initialData.csv", header=FALSE, quote="") 

dim(data)

names(data)
#First thing we see that the variables have no name. We fix this:

colnames(data) <- c("duration","protocol_type","service","flag","src_bytes","dst_bytes",
                        "land","wrong_fragment","urgent","hot","num_failed_logins","logged_in",
                        "num_compromised","root_shell","su_attempted","num_root","num_file_creations",
                        "num_shells","num_access_files","num_outbound_cmds","is_host_login","is_guest_login",
                        "count","srv_count","serror_rate","srv_serror_rate","rerror_rate","srv_rerror_rate",
                        "same_srv_rate","diff_srv_rate","srv_diff_host_rate","dst_host_count","dst_host_srv_count",
                        "dst_host_same_srv_rate","dst_host_diff_srv_rate","dst_host_same_src_port_rate",
                        "dst_host_srv_diff_host_rate","dst_host_serror_rate","dst_host_srv_serror_rate",
                        "dst_host_rerror_rate","dst_host_srv_rerror_rate","attack_type")


