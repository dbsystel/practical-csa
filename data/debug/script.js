const fs = require('fs');
const path = require('path');
const R = require('ramda');

// logError :: Error => Undefined
const logError = err => { if(err) console.error(err); };

if (!process.argv[2] || !fs.existsSync(process.argv[2])) {
  throw 'No path of source JSON or source doesn\'t exist';
}

const BASE_PATH = path.dirname(process.argv[2]);
const data = JSON.parse(fs.readFileSync(process.argv[2]).toString());

// getNextId :: String -> Number
const getNextId = (function() {
  const ids = {};
  return name => {
    if (typeof ids[name] === 'undefined') {
      ids[name] = 0;
    }
    return ++ids[name];
  }
})();

// ensureDouble :: Number -> String
const ensureDouble = R.when(R.gt(10), num => `0${num}`);

// toTime :: Number -> String
const toTime = time => `${ensureDouble(Math.floor(time / 60))}:${ensureDouble(time % 60)}:00`;

// getTime :: { route: Number, from: Number, to: Number } -> Number
const getTime = ({ route, from, to }) =>
  R.find(t =>
    t.route === route && (t.from === from && t.to == to || t.from === to && t.to == from)
  , data.distances);

// routeToCsv :: Route -> String
const routeToCsv = ({ id }) => `${id},R${id},Route ${id},X\n`;

// tripToCsv :: Trip -> String
const tripToCsv = ({ id, route }) => `${route},1,${id},T${id}\n`;

// stopToCsv :: Stop -> String
const stopToCsv = ({ id }) => `${id},Stop ${id},${id}.1,${id}.2,Europe/Berlin\n`;

// stopTimeToCsv :: StopTime -> String
const stopTimeToCsv = ({ trip, arrivalTime, departureTime, stop, seq }) =>
  `${trip},${toTime(arrivalTime)},${toTime(departureTime)},${stop},${seq}\n`;

// stopTransferToCsv :: Stop -> String
const stopTransferToCsv = ({ id, changeTime }) => `${id},${id},2,${changeTime * 60},,\n`;

// footpathToCsv :: Footpath -> String
const footpathToCsv = ({ from, to, minutes }) => `${from},${to},2,${minutes * 60},,\n`;

// Trip :: Interval -> Number -> Trip
const Trip = ({ route, departure, reverse }, time) => {
  const transition = ([from, to]) => ({ route, from, to });
  const routeObj = R.find(R.propEq('id', route), data.routes);
  const stopList = reverse ? R.reverse(routeObj.stops) : routeObj.stops;
  const transitions = R.map(transition, R.aperture(2, stopList));
  const id = getNextId('TRIP');

  let currTime = time + departure;
  let seq = 0;
  let stops = [{
    trip: id,
    stop: transitions[0].from,
    arrivalTime: 0,
    departureTime: currTime,
    seq: seq++,
  }];
  R.forEach(t => {
    currTime += getTime(t).minutes;
    stops.push({
      trip: id,
      stop: t.to,
      arrivalTime: currTime,
      departureTime: ++currTime,
      seq: seq++,
    });
  }, transitions);
  return { id, route, stops };
};

// trips :: [Trip]
const trips = R.chain(route => {
  const intervals = R.filter(R.propEq('route', route.id), data.intervals);

  return R.chain(interval => {
    const t = [];
    for (let time = 0; time < data.maxTime; time += interval.interval) {
      t.push(Trip(interval, time));
    }
    return t;
  }, intervals);
}, data.routes);

const calendarDatesCsv = 'service_id,date,exception_type\n1,20000101,1\n';
fs.writeFile(BASE_PATH + '/calendar_dates.txt', calendarDatesCsv, logError);

const agencyCsv = 'agency_id,agency_name,agency_url,agency_timezone,agency_lang\nX,Agency X,www.example.com,Europe/Berlin,de1,20000101,1\n';
fs.writeFile(BASE_PATH + '/agency.txt', agencyCsv, logError);

const routesCsv = 'route_id,route_short_name,route_long_name,route_type,agency_id\n' +
  data.routes.map(routeToCsv).join('');
fs.writeFile(BASE_PATH + '/routes.txt', routesCsv, logError);

const stopsCsv = 'stop_id,stop_name,stop_lat,stop_lon,stop_timezone\n' +
  data.stops.map(stopToCsv).join('');
fs.writeFile(BASE_PATH + '/stops.txt', stopsCsv, logError);

const tripsCsv = 'route_id,service_id,trip_id,trip_headsign\n' +
  trips.map(tripToCsv).join('');
fs.writeFile(BASE_PATH + '/trips.txt', tripsCsv, logError);

const stopTimesCsv = 'trip_id,arrival_time,departure_time,stop_id,stop_sequence\n' +
  R.chain(R.prop('stops'), trips).map(stopTimeToCsv).join('');
fs.writeFile(BASE_PATH + '/stop_times.txt', stopTimesCsv, logError);

const transfersCsv = 'from_stop_id,to_stop_id,transfer_type,min_transfer_time\n' +
  data.stops.map(stopTransferToCsv).join('') +
  data.footpaths.map(footpathToCsv).join('');
fs.writeFile(BASE_PATH + '/transfers.txt', transfersCsv, logError);
