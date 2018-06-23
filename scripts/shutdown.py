#!/home/jiajun/.py3k/bin/python

import psutil
import logging
import os
import datetime

bat = psutil.sensors_battery()
logging.warn("%s: battery status: %s", datetime.datetime.now(), bat)

if bat.percent < 15:
    logging.warn("gonna shutdown")
    os.system("sudo shutdown -h now")
