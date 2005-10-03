#!/usr/bin/python2.2

import os, os.path, sys, re, time, string

def IsLeapYear(year):
    if year % 4 == 0:
        if year % 100 == 0:
            if year % 400 == 0:
                return 1
            else:
                return 0
        else:
            return 1
    else:
        return 0

def NumberDaysYear(year):
    return 365 + IsLeapYear(year)

def NumberDaysMonth(month = None, year = None):
    if month is None:
        m = time.localtime()[1]
    else:
        m = month

    if year is None:
        y = time.localtime()[0]
    else:
        y = year
    
    if m == 2:
        if IsLeapYear(y):
            return 29
        else:
            return 28
    elif m in (1, 3, 5, 7, 8, 10, 12):
        return 31
    else:
        return 30


class Date(object):
    """The Date class."""
    
    Weekdays = ["Monday",
                "Tuesday",
                "Wednesday",
                "Thursday",
                "Friday",
                "Saturday",
                "Sunday"]

    Months = ["January",
              "February",
              "March",
              "April",
              "May",
              "June",
              "July",
              "August",
              "September",
              "October",
              "November",
              "December"]

    #The slots in a Date object are constrained to allow more efficient operations.
    __slots__ = ["year", "month", "day"]

    def __init__(self, tm = None):
        """The initializer has an optional argument, time, in the time module format,
        wether as in seconds since the epoch (Unix time) wether as a tuple (time tuple).
        If it is not provided, then it returns the current date."""
        if tm is None:
            t = time.localtime()
        else:
            if isinstance(tm, int):
                t = time.localtime(tm)
            else:
                t = tm
                
        self.year, self.month, self.day = t[:3]

    def weekday(self):
        """Returns the weekday of the date.

        The format is as in the time module: Monday is 0 and sunday is 6."""
        a = (14 - self.month)//12
        y = self.year - a
        m = self.month + 12*a -2
        d = (self.day + y + y//4 - y//100 + y//400 + (31*m//12))%7
        if d:
            ret = d - 1
        else:
            ret = 6
        return ret

    def __str__(self):
        return "%s, %d-%s-%d" % (Date.Weekdays[self.weekday()],
                                 self.day,
                                 Date.Months[self.month - 1],
                                 self.year)

    def copy(self):
        """Deep copy of Date objects."""
        ret = Date()
        ret.year, ret.month, ret.day = self.year, self.month, self.day
        return ret

    #The iterator protocol. The iteration is "destructive", like in files.
    def __iter__(self):
        return self

    def next(self):
        #Last day of the month.
        if self.day == NumberDaysMonth(self.month, self.year):
            self.day = 1
            #December case.
            if self.month == 12:
                self.month = 1
                self.year += 1
            else:
                self.month += 1
        else:
            self.day += 1

    #Extended iterator protocol. One can go backwards.
    def previous(self):
        #First day of the month.
        if self.day == 1:
            #January case.
            if self.month == 1:
                self.month = 12
                self.year -= 1
            else:
                self.month -= 1
            self.day = NumberDaysMonth(self.month, self.year)
        else:
            self.day -= 1

    #Comparison methods.
    def __eq__(self, date):
        return self.year == date.year and self.month == date.month and\
               self.day == date.day

    def __lt__(self, other):
        return (self.year, self.month, self.day) < (other.year, other.month, other.day)

    def __le__(self, other):
        return (self.year, self.month, self.day) <= (other.year, other.month, other.day)

    #Dates can be used as keys in dictionaries.
    def __hash__(self):
        return hash((self.year, self.month, self.day))

    #Some useful methods.
    def GetYearDay(self):
        """Returns the year day of a date."""
        ret = self.day
        for month in range(1, self.month):
            ret += NumberDaysMonth(month, self.year)
        return ret

    def DaysToEndYear(self):
        """Returns the number of days until the end of the year."""
        ret = NumberDaysMonth(self.month, self.year) - self.day
        for i in range(self.month + 1, 13):
            ret += NumberDaysMonth(i, self.year)
        return ret

    def GetWeekday(self):
        """Returns the weekday of the date in string format."""
        return Date.Weekdays[self.weekday()]

    def GetMonth(self):
        """Returns the month of the date in string format."""
        return Date.Months[self.month - 1]

    def ToJDNumber(self):
        """Returns the Julian day number of a date."""
        a = (14 - self.month)//12
        y = self.year + 4800 - a
        m = self.month + 12*a - 3
        return self.day + ((153*m + 2)//5) + 365*y + y//4 - y//100 + y//400 - 32045

    #Binary operations.
    def __add__(self, n):
        """Adds a (signed) number of days to the date."""
        if isinstance(n, int):
            #Calculate julian day number and add n.
            temp = self.ToJDNumber() + n
            #Convert back to date format.
            return DateFromJDNumber(temp)
        else:
            raise TypeError, "%s is not an integer." % str(n)

    def __sub__(self, date):
        """Returns the (signed) difference of days between the dates."""
        #If it is an integer defer calculation to the __add__ method.
        if isinstance(date, int):
            return self.__add__(-date)
        elif isinstance(date, Date):
            #Case: The years are equal.
            if self.year == date.year:
                return self.GetYearDay() - date.GetYearDay()
            else:
                if self < date:
                    ret = self.DaysToEndYear() + date.GetYearDay()
                    for year in range(self.year + 1, date.year):
                        ret += NumberDaysYear(year)
                    return -ret
                else:
                    ret = date.DaysToEndYear() + self.GetYearDay()
                    for year in range(date.year + 1, self.year):
                        ret += NumberDaysYear(year)
                    return ret
        else:
            raise TypeError, "%s is neither an integer nor a Date." % str(date)

    #Adding an integer is "commutative".
    def __radd__(self, n):
        return self.__add__(n)

    #Conversion methods.
    def ToTimeTuple(self):
        """Convert a date into a time tuple (time module) corresponding to the
        same day with the midnight hour."""
        ret = [self.year, self.month, self.day]
        ret.extend([0, 0, 0])
        ret.append(self.weekday())
        ret.extend([self.GetYearDay(), 0])
        return tuple(ret)


def needparsep(fname):
    import re
    import time
    curdate = Date()
    if re.search("^([0-9]{4})\.([0-9]+)\.([0-9]+)$", fname):
        if curdate <= Date(time.strptime(fname, "%Y.%m.%d")):
            return True
        else:
            return False
    else:
        return False

def needtransferp(line):
    import re
    if re.search("^\s*([0-9]+:[0-9]+)\s*(.+)", line):
        return True
    else:
        return False
    
if __name__ == "__main__":

    try:
        dirname = sys.argv[1]
    except IndexError:
        dirname = "~/emacs/plans"

    filelist = filter(needparsep, os.listdir(os.path.expanduser(dirname)))

    selected = []
    for filename in filelist:
        thatdate = Date(time.strptime(filename, "%Y.%m.%d"))
        thatdate = repr(thatdate.month)+"/"+repr(thatdate.day)+"/"+repr(thatdate.year)
        
        selected = filter(needtransferp, open(os.path.join(os.path.expanduser(dirname), filename), "r"))
        for line in selected:
            m = re.search("^\s*([0-9]+:[0-9]+)\s*\|\s*([0-9]+:[0-9]+)\s*\|\s*(.+)", line)
            if m:
                outputstr = thatdate+" "+m.group(1)+"-"+m.group(2)+" "+m.group(3)
            else:
                m = re.search("^\s*([0-9]+:[0-9]+)\s*\|\s*(.+?)(\s+\(([0-9]+:[0-9]+)\))?\s*$", line)
                if m.group(4):
                    start = m.group(1)
                    duration = m.group(4)
                    s = re.search("([0-9]+):([0-9]+)", start)
                    starthour = int(s.group(1))
                    startmin = int(s.group(2))
                    s = re.search("([0-9]+):([0-9]+)", duration)
                    min = startmin+int(s.group(2))
                    hour = starthour+int(s.group(1))
                    if min >= 60:
                        min -= 60
                        hour += 1
                    if hour >= 24:
                        hour -= 24
                    min = string.zfill(repr(min), 2)
                    hour = string.zfill(repr(hour), 2)
                    outputstr = thatdate+" "+m.group(1)+"-"+hour+":"+min+" "+m.group(2)
                else:
                    outputstr = thatdate+" "+m.group(1)+" "+m.group(2)
            print outputstr

