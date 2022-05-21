// Adapted from Flocking Processing example by Daniel Schiffman:
// http://processing.org/learning/topics/flocking.html

var steer = new Point()


var Boid = Base.extend({
    initialize: function(position, maxSpeed, maxForce) {
        var strength = Math.random() * 0.5;
        this.acceleration = new Point();
        this.vector = Point.random() * 2 - 1;
        this.position = position;
        this.radius = 30;
        this.maxSpeed = maxSpeed + strength;
        this.maxForce = maxForce + strength;
        this.createItems();
    },

    run: function(boids, temp) {
        this.flock(boids, temp);
        this.borders();
        this.update();
        this.calculateTail();
    },

    calculateTail: function() {

        // var point = this.position;
        var segment = this.path.firstSegment;
        segment.next.point = segment.point;
        segment.point = this.position;
        // var vector = segment.point - segment.next.point;
        // vector.length = 5;
        // segment.next.point = segment.point - vector;
    },

    createItems: function() {
        this.path = new Path({
            strokeColor: Color.random(),
            strokeWidth: 2,
            strokeCap: 'round'
        });
        for (var i = 0; i < 2; i++)
            this.path.add(new Point(this.position));
    },

    // We accumulate a new acceleration each time based on three rules
    flock: function(boids, temp) {
        var separation = this.separate(boids, temp) * 1.5;
        var alignment = this.align(boids, temp) * 1.5;
        var cohesion = this.cohesion(boids, temp);
        this.acceleration += separation + alignment + cohesion;
    },

    update: function() {
        // Update velocity
        this.vector += this.acceleration;
        // Limit speed (vector#limit?)
        if (this.vector.length > this.maxSpeed) {
            this.vector.length = this.maxSpeed;
        }
        this.position += this.vector;
        // Reset acceleration to 0 each cycle
        this.acceleration = new Point();
    },

    seek: function(target) {
        this.acceleration += this.steer(target, false);
    },

    arrive: function(target) {
        this.acceleration += this.steer(target, true);
    },

    borders: function() {
        var vector = new Point();
        var position = this.position;
        var radius = this.radius;
        var size = view.size;
        if (position.x < -radius) vector.x = size.width + radius;
        if (position.y < -radius) vector.y = size.height + radius;
        if (position.x > size.width + radius) vector.x = -size.width -radius;
        if (position.y > size.height + radius) vector.y = -size.height -radius;
        if (!vector.isZero()) {
            this.position += vector;
            var segments = this.path.segments;
            segments[0].point += vector;
            segments[1].point += vector;
        }
    },

    // A method that calculates a steering vector towards a target
    // Takes a second argument, if true, it slows down as it approaches
    // the target
    steer: function(target, slowdown) {
        var steer,
            desired = target - this.position;
        var distance = desired.length;
        // Two options for desired vector magnitude
        // (1 -- based on distance, 2 -- maxSpeed)
        if (slowdown && distance < 100) {
            // This damping is somewhat arbitrary:
            desired.length = this.maxSpeed * (distance / 100);
        } else {
            desired.length = this.maxSpeed;
        }
        steer = desired - this.vector;
        if (this.maxForce < steer.length) {
            steer.length = this.maxForce;
        }
        return steer;
    },

    separate: function(boids, temp) {
        var desiredSeperation = 30;
        steer.x = 0;
        steer.y = 0;
        var count = 0;
        // For every boid in the system, check if it's too close
        for (var i = 0; i < N; i++) {
            // var vector = new Point();
            var vector = this.position - boids[i].position;
            var distance = vector.length;
            temp[i] = distance;
            if (distance > 0 && distance < desiredSeperation) {
                // Calculate vector pointing away from neighbor
                steer += vector.normalize(1 / distance);
                count++;
            }
        }
        // Average -- divide by how many
        if (count > 0)
            steer /= count;
        if (!steer.isZero()) {
            // Implement Reynolds: Steering = Desired - Velocity
            steer.length = this.maxSpeed;
            steer -= this.vector;
            if (this.maxForce < steer.length) {
                steer.length = this.maxForce;
            }
        }
        return steer;
    },

    // Alignment
    // For every nearby boid in the system, calculate the average velocity
    align: function(boids, temp) {
        var neighborDist = 25;
        steer.x = 0;
        steer.y = 0;
        var count = 0;
        for (var i = 0, l = N; i < l; i++) {
            var distance = temp[i];
            if (distance > 0 && distance < neighborDist) {
                steer += boids[i].vector;
                count++;
            }
        }

        if (count > 0)
            steer /= count;
        if (!steer.isZero()) {
            // Implement Reynolds: Steering = Desired - Velocity
            steer.length = this.maxSpeed;
            steer -= this.vector;
            if (this.maxForce < steer.length) {
                steer.length = this.maxForce;
            }
        }
        return steer;
    },

    // Cohesion
    // For the average location (i.e. center) of all nearby boids,
    // calculate steering vector towards that location
    cohesion: function(boids, temp) {
        var neighborDist = 100;
        steer.x = 0;
        steer.y = 0;
        var count = 0;
        for (var i = 0, l = N; i < l; i++) {
            var distance = temp[i];
            if (distance > 0 && distance < neighborDist) {
                steer += boids[i].position; // Add location
                count++;
            }
        }
        if (count > 0) {
            steer /= count;
            // Steer towards the location
            return this.steer(steer, false);
        }
        return steer;
    }
});

var N = 100;
var boids = Array(N);
var temp = new Float32Array(N);

// Add the boids:
for (var i = 0; i < N; i++) {
  var position = Point.random() * view.size;
  boids[i] = new Boid(position, 6, 0.01);
}

enabled = true;


function onFrame(event) {
    if (enabled) {
        for (var i = 0, l = N; i < l; i++) {
            boids[i].run(boids,temp);
        }
    }
}

function onKeyDown(event) {
    if (event.key == 'space') {
        enabled = !enabled
        return false;
    }
}
