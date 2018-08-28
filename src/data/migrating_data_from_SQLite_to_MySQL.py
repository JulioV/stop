import pandas as pd
import argparse
import os
import subprocess
from dotenv import find_dotenv, load_dotenv
from pathlib import Path
from sqlalchemy import create_engine

def create_mysql_engine():
    database = os.environ.get("database")    
    user = os.environ.get("user")
    password = os.environ.get("password")
    host = os.environ.get("host")
    port = os.environ.get("port")
    return create_engine('mysql+mysqlconnector://{}:{}@{}:{}/{}'.format(user, password, host, port, database), echo=False)


# Taken from https://www.ryanbaumann.com/blog/2016/4/30/python-pandas-tosql-only-insert-new-rows
def clean_df_db_dups(df, tablename, engine, dup_cols=[],
                         filter_continuous_col=None, filter_categorical_col=None):
    """
    Remove rows from a dataframe that already exist in a database
    Required:
        df : dataframe to remove duplicate rows from
        engine: SQLAlchemy engine object
        tablename: tablename to check duplicates in
        dup_cols: list or tuple of column names to check for duplicate row values
    Optional:
        filter_continuous_col: the name of the continuous data column for BETWEEEN min/max filter
                               can be either a datetime, int, or float data type
                               useful for restricting the database table size to check
        filter_categorical_col : the name of the categorical data column for Where = value check
                                 Creates an "IN ()" check on the unique values in this column
    Returns
        Unique list of values from dataframe compared to database table
    """
    args = 'SELECT %s FROM %s' %(', '.join(['{0}'.format(col) for col in dup_cols]), tablename)
    args_contin_filter, args_cat_filter = None, None
    
    if filter_continuous_col is not None:
        if df[filter_continuous_col].dtype == 'datetime64[ns]' or df[filter_continuous_col].dtype:
            args_contin_filter = """ %s BETWEEN %s AND %s""" %(filter_continuous_col,
                              df[filter_continuous_col].min(), df[filter_continuous_col].max())


    if filter_categorical_col is not None:
        args_cat_filter = ' %s in (%s)' %(filter_categorical_col,
                          ', '.join(["'{0}'".format(value) for value in df[filter_categorical_col].unique()]))

    if args_contin_filter and args_cat_filter:
        args += ' Where ' + args_contin_filter + ' AND' + args_cat_filter
    elif args_contin_filter:
        args += ' Where ' + args_contin_filter
    elif args_cat_filter:
        args += ' Where ' + args_cat_filter

    # print(args)
    df.drop_duplicates(dup_cols, keep='last', inplace=True)
    df = pd.merge(df, pd.read_sql(args, engine), how='left', on=dup_cols, indicator=True)
    df = df[df['_merge'] == 'left_only']
    df.drop(['_merge'], axis=1, inplace=True)
    return df

def main(args):
    load_dotenv(find_dotenv())

    engine = create_mysql_engine()

    if args.sqlite:
        for directory in args.sqlite:
            pathlist = Path(directory).glob('**/*.db')
            for db in pathlist:
                get_sqlite_tables = "sqlite3 {} \"SELECT tbl_name FROM sqlite_master WHERE type='table' and tbl_name not like 'sqlite_%' AND tbl_name not like 'android_metadata' AND tbl_name not like 'scheduler';\"".format(db)
                tables = subprocess.check_output(get_sqlite_tables, shell=True)
                for table in tables.decode('ascii').splitlines():
                    print("Processing table {} from {}".format(table, db))

                    # SQLite to CSV
                    csv_path = db.parent / (table + ".csv")
                    table_to_csv = "sqlite3 -header -csv {} \"select * from {};\" > {}".format(db, table, csv_path)
                    os.system(table_to_csv)

                    if os.stat(csv_path).st_size > 0:
                        df = pd.read_csv(csv_path, index_col="_id")
                        df_unique = clean_df_db_dups(df, table, engine, dup_cols=["timestamp", "device_id"],
                            filter_continuous_col="timestamp", filter_categorical_col="device_id")

                        print("Unique elemnts found to insert: {}".format(df_unique.shape[0]))
                        if df_unique.shape[0] > 0:
                            # This is necessary as the tables' autoincrement might be corrupted? Need to recreat _id column to reset auto_increment 
                            # engine.execute("alter table {} drop _id;".format(table))
                            # engine.execute("alter table {} add _id int(11) NOT NULL AUTO_INCREMENT PRIMARY KEY;".format(table))

                            # Insert all unique values
                            df_unique.to_sql(table, engine, if_exists='append', index=False, chunksize=10000)

                            print("New rows inserted {}".format(df_unique.shape[0]))
                    else:
                        print("Empty table")
                    print("")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-se','--sqlite', nargs='+', help='list of folders with sqlite databases', required=True)
    args = parser.parse_args()
    main(args)