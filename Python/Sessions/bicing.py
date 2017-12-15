import urllib.request
import xml.etree.ElementTree as ET

URL_RESOURCE = 'http://wservice.viabicing.cat/getstations.php?v=1'

sock = urllib.request.urlopen(URL_RESOURCE)
xml_source = sock.read().decode('utf-8')
sock.close()

min_bikes = int(input('Minimum number of bikes? '))

root = ET.fromstring(xml_source)
for station in root.findall('station'):
    bikes = station.find('bikes').text;
    if int(bikes) >= min_bikes:
        station_data = {
            'id': station.find('id').text,
            'street': station.find('street').text,
            'number': station.find('streetNumber').text,
            'slots': station.find('slots').text,
            'bikes': bikes
        }
        print('Bike station {id} at {street} {number} with {bikes} bikes and {slots} slots'
                .format(**station_data))
