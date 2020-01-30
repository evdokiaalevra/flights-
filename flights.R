#Εργασία 1
########################################################3
#Κάτω από κάθε ερώτηση να τοποθετήσετε το κώδικα-απάντηση της αντίστοιχης ερώτησης
#Μπορείτε για κάθε απάντηση να χρησιμοποιήσετε οποιοδήποτε μοτίβο κώδικα έχετε διδαχθεί
#An den emfanizontai sosta ta ellinika epilegetai apo to menu tools->global options->code->saving->default code encoding->utf-8
#epeita epilegetai apply kleinete to arxeio kai to ksanaanoigete

#Να υπολογίσετε και να εμφανίσετε τις απαντήσεις για κάθε ένα από τα παρακάτω ερωτήματα
library(tidyverse)
str(DelayedFlights)
summary(DelayedFlights)

#Ερώτηση 1:να βρείτε (αν υπάρχουν) και να εμφανίσετε το πλήθος των κενών γραμμών σε κάθε στήλη του dataset
is.na.data.frame(DelayedFlights)
colSums(is.na(DelayedFlights))

#Ερώτηση 2:να υπολογίσετε και να εμφανίσετε ποια ημέρα σε ποιον μήνα σημειώθηκαν οι περισσότερες καθυστερήσεις πτήσεων
f<-DelayedFlights
f<-filter(f,f$ArrDelay>0)
f<-group_by(f,f$Month,f$DayofMonth)
f<-tally(f)
which.max(f$n)
rm(f)

#Ερώτηση 3: να υπολογίσετε και να εμφανίσετε τον ημερήσιο μέσο όρο καθυστερήσεων για καθέναν από τους θερινούς μήνες του 2008
f<-DelayedFlights
f%>%
  filter(f$Month>=6,f$Month<=8,f$ArrDelay>=0)%>%
  group_by(Month)%>%
  tally()%>%
  mutate(numberofdays=c(30,31,31))%>%
  mutate(dayilyavg=n/numberofdays)
rm(f)

#Ερώτηση 4: να υπολογίσετε και να εμφανίσετε το όνομα της αεροπορικής εταιρίας που είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β
f<-DelayedFlights
f%>%
  filter(CancellationCode=="B")%>%
  group_by(UniqueCarrier)%>%
  tally()%>%
  arrange(desc(n))%>%
  head(1)
#To όνομα της αεροπορικής εταιρίας είναι "MQ"

#Ερώτηση 5: να βρείτε τους κωδικούς των πτήσεων με τον μεγαλύτερο αριθμό καθυστερήσεων
f%>%
  filter(ArrDelay>0)%>%
  group_by(FlightNum)%>%
  tally()%>%
  arrange(desc(n))
rm(f)

#Ερώτηση 6: να βρείτε και να υπολογίσετε το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις
f<-DelayedFlights
f%>%
  filter(ArrDelay>0,Distance==max(Distance))%>%
  group_by(Dest,Distance)%>%
  tally()%>%
  arrange(desc(n))%>%
  head(1)
#Το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις είναι "ΗNL"

#Ερώτηση 7: να βρείτε και να εμφανίσετε τους προορισμούς που είχαν την μεγαλύτερη καθυστέρηση (πτήσεις που εκτελέστηκαν)
f%>%
  select(Dest,ArrDelay,Cancelled)%>%
  filter(Cancelled=="0",ArrDelay>0)%>%
  arrange(desc(ArrDelay))%>%
  group_by(Dest)

#Ερώτηση 8: να βρείτε και να εμφανίσετε το όνομα της αεροπορικής εταιρείας που είχε τις μεγαλύτερες καθυστερήσεις που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών
f%>%
  filter(LateAircraftDelay>0)%>%
  group_by(UniqueCarrier)%>%
  summarise(sumDelays=sum(LateAircraftDelay))%>%
  arrange(desc(sumDelays))%>%
  head(1)
#Η αεροπορική εταιρία που είχε συνολικά τις μεγαλύτερες καθυστερήσεις που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών είναι η "WN".

#Ερώτηση 9: να υπολογίσετε πόσες ακυρώσεις πτήσεων τύπου Α σημειώθηκαν την 13η ημέρα κάθε μήνα
f%>%
  filter(CancellationCode=="A",DayofMonth==13)%>%
  group_by(Month)%>%
  tally()
#Καθυστερήσεις τύπου Α την 13η μέρα του μήνα σημειώθηκαν τον Νοέμβριο(2) και τον Δεκέμβριο(3)

#Ερώτηση 10: υπολογίσετε και να εμφανίσετε την μέση καθυστέρηση πτήσεων που εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008
f%>%
  filter(Month==4,DayofMonth>=10,DayofMonth<=23,ArrDelay>0)%>%
  summarise(meanDelay=mean(ArrDelay))

#Ερώτηση 11: να υπολογίσετε και να εμφανίσετε τον μήνα που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε έλεγχους ασφαλείας κατά τις ώρες 06.00-14.00
f%>%
  filter(SecurityDelay>0,ArrTime>=600,ArrTime<=1400)%>%
  select(Month,SecurityDelay)%>%
  group_by(Month)%>%
  arrange(desc(SecurityDelay))%>%
  head(1)

#Ο μήνας που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε ελέγχους ασφαλείας κατα τις ώρες 06.00-14.00 είναι ο Ιούλιος.

#Ερώτηση 12: να υπολογίσετε και να εμφανίσετε ποιος κωδικός πτήσης(αριθμός πτήσης) είχε το πρώτο δεκαήμερο του Νοεμβρίου του 2008 την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της
f%>%
  filter(Month==11,DayofMonth>=1,DayofMonth<=10,ArrDelay<0)%>%
  select(FlightNum,ArrDelay)%>%
  group_by(FlightNum)%>%
  arrange(ArrDelay)%>%
  head(1)

#Ερώτηση 13: να υπολογίσετε και να εμφανίσετε ποιο αεροδρόμιο (τοποθεσία αναχώρησης) είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 τις περισσότερες πτήσεις με καθυστέρηση(αναχωρίσεων) μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς
f%>%
  filter(Month==8,DayofMonth>=11,DayofMonth<=20,LateAircraftDelay>30)%>%
  group_by(Origin)%>%
  tally()%>%
  arrange(desc(n))%>%
  head(1)
#Ερώτηση 14: να βρείτε και να εμφανίσετε τις πτήσεις που εκτράπηκαν από την πορεία τους αλλά ολοκληρώθηκαν καθώς και τον συνολικό χρόνο που απαιτήθηκε
f%>%
  filter(Diverted==1,Cancelled==0)%>%
  select(FlightNum,CRSElapsedTime)%>%
  group_by(FlightNum)%>%
  arrange(desc(CRSElapsedTime))

#Ερώτηση 15: ποιος μήνας είχε την μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας"). Ως απόκλιση να θεωρηθεί η διαφορά ανάμεσα στον προγραμματισμένο και τον πραγματικό χρόνο εκτέλεσης της πτήσης
f%>%
  filter(CRSElapsedTime>0,ActualElapsedTime>0,ArrDelay>0)%>%
  group_by(Month)%>%
  summarise(tupikiapoklisi=sd(ActualElapsedTime-CRSElapsedTime,na.rm = TRUE))%>%
  arrange(desc(tupikiapoklisi))%>%
  head(1)

#Ο Ιούλιος.





