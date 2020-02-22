library(dbConnect)
library(RODBC)

# Connect to database stored in local
con=dbConnect(MySQL(),
                  user="root",
                  host="127.0.0.1",
                  dbname="ubiqum",
                  password="Totorigolo1#")


# Create table for Brand
dbSendQuery(con, "CREATE TABLE IF NOT EXISTS brand (
                  client_id INT AUTO_INCREMENT,
                  salary INT,
                  age INT,
                  elevel VARCHAR(50),
                  car VARCHAR(30),
                  zipcode INT,
                  credit INT,
                  brand VARCHAR(30),
                  PRIMARY KEY (client_id)
                  );"
            )

# Drop table
dbSendQuery(con, "SELECT * FROM Car;")


# Insert row in database
dbSendQuery(con, "INSERT INTO brand 
            (salary, age, elevel, car, zipcode, credit, brand) 
            VALUES 
            ('121212', '34', 'university', 'BMW', '678678', '982177', 'ACER')"
            )

# Print content
head(dbGetQuery(con, "DROP TABLE brand"))

# Disconnect to database
dbDisconnect(con)

# Save dataframe in table
# sudo nano /etc/mysql/my.cnf
# sudo /etc/init.d/mysql restart

dbWriteTable(con, name = "Brand", value = Response, row.names = FALSE)

dbReadTable(con, "Brand")

