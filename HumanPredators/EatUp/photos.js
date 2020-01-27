"use strict";

// The PhotoSeq class represents a random sequence of photos which
// make up a single trial, with a current photo.
class PhotoSeq {
    /** 
     * Constructs a PhotoSeq instance. Randomly selects numToShow photos from photos.
     *
     * @param {array} candidatePhotos Array of photos to select from. If photos contain a
     *        weight field, it will be used adjust the random
     *        selection. Higher weights are more likely to be selected.
     */
    constructor(candidatePhotos, numToShow) {
        this.index = 0;
        this.photos = this.selectRandom(candidatePhotos, numToShow);
    }

    // Progresses the current photo to the next photo in the trial
    get moveToNext() {
        return ++this.index < this.photos.length;
    }
    get currentPhoto() { return this.photos[this.index]; }
    get hasCurrentPhoto() { return this.index < this.photos.length; }
    get percentComplete() { return 100 * (this.index + 1) / this.photos.length; }

    // Used internally. Selects the photos to be used in the trial
    // from the list of candidate photos
    selectRandom(photos, numToShow) {
        shuffleArray(photos);
        var result = [];
        // Pick out numToShow photos, based on optional weighting
        // Calculate total weight
        let total = 0;
        for (let i = photos.length - 1; i >= 0; i--) {
            // Assume weight of 1 if not specified
            total += photos[i].weight || 1;
            photos[i].cumulative = total;
        }
        // Calculate weight change between photos
        let inc = total / numToShow;
        for (let i = photos.length - 1; i >= 0; i--) {
            if (photos[i].cumulative >= result.length * inc) {
                result.push(photos[i]);
            }
        }
        return result;
    }
};

function shuffleArray(a) {

    for (let i = a.length - 1; i > 0; i--) {
        let j = Math.floor(Math.random() * (i + 1)); // Random index from 0 to i
        [a[i], a[j]] = [a[j], a[i]]                  // Swap elements
    }
}

const LoadImage = url => {
    return new Promise((resolve, reject) => {
        var image = new Image();
        image.addEventListener('load', () => resolve(image));
        image.addEventListener('error', reject);
        image.src = url;
    });
}
    
