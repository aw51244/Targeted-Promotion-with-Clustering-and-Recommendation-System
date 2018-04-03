library(data.table)
# read in data
transaction_table <- fread("transaction_table.csv")
transaction_table[,tran_yr:=substr(tran_dt,1,4)]

# join two tables
product_table <- fread("product_table.csv")
setkey(transaction_table,prod_id)
setkey(product_table,prod_id)
whole_table <-product_table[transaction_table]
# pick 2017
whole_table_2017<-whole_table[tran_yr=="2017"]

# Extract features on category
category_list <- unique(whole_table_2017$category_id)
# save the category list
category_tb <- data.table(category_id=category_list, id=1:430)
setkey(category_tb,'category_id')
product_tb <- product_table[,.(category_id,category_desc_eng)]
setkey(product_tb,'category_id')
category_tb <- unique(product_tb[category_tb][order(id)])
fwrite(category_tb,"category_list.csv")

# Extract features on how much did customer spend on each category in 2017

cust_2017<- whole_table_2017[,.(Sum_2017=sum(tran_prod_paid_amt)),by =.(cust_id)]
setkey(cust_2017,cust_id)
dim(cust_2017)

for (i in 1:length(category_list)){
  tmp <- whole_table_2017[category_id==category_list[i],
                                      .(cust_id, Sum = sum(tran_prod_paid_amt)),by =.(cust_id)]
  setkey(tmp,cust_id)
  cust_2017 <- tmp[cust_2017]
}

cust_2017[, grep("cust_id.", names(cust_2017)) := NULL]
names(cust_2017)
dim(cust_2017)

# Extract features on subcategory
subcategory_list <- unique(whole_table_2017$subcategory_id)
# save the sub_category list
subcategory_tb <- data.table(subcategory_id=subcategory_list, id=1:1476)
setkey(subcategory_tb,'subcategory_id')
product_tb <- product_table[,.(subcategory_id,sub_category_desc)]
setkey(product_tb,'subcategory_id')
subcategory_tb <- unique(product_tb[subcategory_tb][order(id)])
fwrite(subcategory_tb,"subcategory_list.csv")

# Extract features on how much did customer spend on each subcategory in 2017

for (i in 1:length(subcategory_list)){
  tmp <- whole_table_2017[subcategory_id==subcategory_list[i],
                          .(cust_id, SubSum = sum(tran_prod_paid_amt)),by =.(cust_id)]
  setkey(tmp,cust_id)
  cust_2017 <- tmp[cust_2017]
}

cust_2017[, grep("cust_id.", names(cust_2017)) := NULL]
names(cust_2017)
dim(cust_2017)


# exclude who spend less than 5k in 2017
cust_2017 <- cust_2017[Sum_2017>=5000]
dim(cust_2017)

# exclude who have not used discount in 2017
discount_2017 <- whole_table_2017[,.(total_discount=sum(tran_prod_discount_amt)),by=.(cust_id)]
discount_2017[order(-total_discount)]
max(discount_2017$total_discount) # nobody have, so exclude no one

fwrite(cust_2017,"cust_2017.csv")


# After clustering and recommendering subcategories in python, 
# we need to find which brands are popular in each popular sub-categories.
# compute most popular brand in each subcategory
whole_table_2017[sub_category_desc=="POLVO CONGELADO EMB",
                 .(spend_POLVO=sum(tran_prod_paid_amt)),
                 by = .(brand_desc)][order(-spend_POLVO)]

whole_table_2017[subcategory_id==93580,
                 .(spend_sub=sum(tran_prod_paid_amt)),
                 by = .(brand_desc)][order(-spend_sub)]

whole_table_2017[subcategory_id==94735,
                 .(spend_sub=sum(tran_prod_paid_amt)),
                 by = .(brand_desc)][order(-spend_sub)]


whole_table_2017[subcategory_id==94642,
                 .(spend_sub=sum(tran_prod_paid_amt)),
                 by = .(brand_desc)][order(-spend_sub)]

whole_table_2017[subcategory_id==94043,
                 .(spend_sub=sum(tran_prod_paid_amt)),
                 by = .(brand_desc)][order(-spend_sub)]

whole_table_2017[subcategory_id==90432,
                 .(spend_sub=sum(tran_prod_paid_amt)),
                 by = .(brand_desc)][order(-spend_sub)]
