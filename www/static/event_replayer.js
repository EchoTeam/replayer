(function($) {
    if(!window.EventReplayer) window.EventReplayer = {};
    EventReplayer.Control = function(){};
    EventReplayer.Control.prototype.init = function() {
        var self = EventReplayer.control;
        if ("MozWebSocket" in window) {
            WebSocket = MozWebSocket;
        }
        if(!("WebSocket" in window)) {
            self.showError("Websockets are not supported");
        } else {
            self.setConnectionStatus("warning", "connecting");
            self.connect();
        }
    };
    EventReplayer.Control.prototype.connect = function() {
        var self = EventReplayer.control;
        self.websocket = new WebSocket("ws://" + location.host + "/websocket");
        self.websocket.onopen = self.onopen;
        self.websocket.onclose = self.onclose;
        self.websocket.onmessage = self.onmessage;
        self.websocket.onerror = self.onerror;
    };
    EventReplayer.Control.prototype.onopen = function(evt) {
        var self = EventReplayer.control;
        self.setConnectionStatus("success", "connected");
        self.websocket.send(JSON.stringify({"request": "get_stats"}));
    };
    EventReplayer.Control.prototype.onclose = function(evt) {
        var self = EventReplayer.control;
        self.setConnectionStatus("important", "disconnected");
    };
    EventReplayer.Control.prototype.onmessage = function(evt) {
        var self = EventReplayer.control;
        var data = JSON.parse(evt.data);
        if (data.stats && self.stats != JSON.stringify(data.stats)) {
            $('#tasksFile').html('Tasks file: ' + data.stats.file);
            $('#workersNum').html('Workers number per node: ' + data.stats.workers_num);
            var ringnodes = '';
            var nodestats = '';
            var total_tasks_count = 0;
            var total_tasks_processed = 0;
            var total_tasks_failed = 0;
            var total_reply_errors = 0;
            var total_ok_messages = {};
            var total_error_messages = {};
            $.each(data.stats.ring, function(i, node) {
                var nodedata = data.stats.nodes[node];
                ringnodes += (i ? ', ' : '') + node;
                if (nodedata && nodedata.counters && nodedata.counters.tasks_count) {
                    total_tasks_count += nodedata.counters.tasks_count;
                    total_tasks_processed += nodedata.counters.tasks_processed;
                    total_tasks_failed += nodedata.counters.tasks_failed;
                    total_reply_errors += nodedata.counters.reply_errors;
                    nodestats += '<h4>' + node  + '</h4>';
                    nodestats += '<div class="well">';
                    nodestats += '  <div class="progress progress-striped active">';
                    var success = Math.round(100 * (nodedata.counters.tasks_processed - nodedata.counters.reply_errors) / nodedata.counters.tasks_count);
                    var warning = Math.round(100 * nodedata.counters.reply_errors / nodedata.counters.tasks_count);
                    var danger = Math.round(100 * nodedata.counters.tasks_failed / nodedata.counters.tasks_count);
                    nodestats += '    <div class="bar bar-success" style="width: ' + success + '%;"></div>';
                    nodestats += '    <div class="bar bar-warning" style="width: ' + warning + '%;"></div>';
                    nodestats += '    <div class="bar bar-danger" style="width: ' + danger + '%;"></div>';
                    nodestats += '  </div>';
                    nodestats += '  <div class="pull-right">';
                    nodestats += '    <small>';
                    nodestats += '    Successed: ' + nodedata.counters.tasks_processed + ', Failed: ' + nodedata.counters.tasks_failed + ', Reply errors: ' + nodedata.counters.reply_errors + ', Total: ' + nodedata.counters.tasks_count;
                    nodestats += '    </small>';
                    nodestats += '  </div>';
                    var ok_messages = '';
                    $.each(nodedata.ok_messages, function(sc, sv) {
                        ok_messages += sc + ': ' + sv + '<br/>';
                        if (!total_ok_messages[sc]) {
                            total_ok_messages[sc] = 0;
                        }
                        total_ok_messages[sc] += sv;
                    });
                    if (ok_messages) {
                        nodestats += '<h5>OK Messages</h5>' + ok_messages;
                    };

                    var error_messages = '';
                    $.each(nodedata.error_messages, function(sc, sv) {
                        error_messages += sc + ': ' + sv + '<br/>';
                        if (!total_error_messages[sc]) {
                            total_error_messages[sc] = 0;
                        }
                        total_error_messages[sc] += sv;
                    });
                    if (error_messages) {
                        nodestats += '<h5>ERROR Messages</h5>' + error_messages;
                    };
                    nodestats += '</div>';
                }
            });
            $('#ringNodes').html('Ring: [' + ringnodes + ']');
            $('#nodeStats').html(nodestats == '' ? 'There is no statistics' : nodestats);
            if (total_tasks_count) {
                var overallstats = '';
                var success = Math.round(100 * (total_tasks_processed - total_reply_errors) / total_tasks_count);
                var warning = Math.round(100 * total_reply_errors / total_tasks_count);
                var danger = Math.round(100 * total_tasks_failed / total_tasks_count);
                overallstats += '<div class="progress progress-striped active">';
                overallstats += '    <div class="bar bar-success" style="width: ' + success + '%;"></div>';
                overallstats += '    <div class="bar bar-warning" style="width: ' + warning + '%;"></div>';
                overallstats += '    <div class="bar bar-danger" style="width: ' + danger + '%;"></div>';
                overallstats += '  </div>';
                overallstats += '  <div class="pull-right">';
                overallstats += '    <small>';
                overallstats += '    Successed: ' + total_tasks_processed + ', Failed: ' + total_tasks_failed + ', Reply errors: ' + total_reply_errors + ', Total: ' + total_tasks_count;
                overallstats += '    </small>';
                overallstats += '  </div>';
                var okstatuses = '';
                $.each(total_ok_messages, function(sc, sv) {
                    okstatuses += sc + ': ' + sv + '<br/>';
                });
                if (okstatuses) {
                    overallstats += '<h5>OK messages</h5>' + okstatuses;
                };
                var errorstatuses = '';
                $.each(total_error_messages, function(sc, sv) {
                    errorstatuses += sc + ': ' + sv + '<br/>';
                });
                if (errorstatuses) {
                    overallstats += '<h5>ERROR messages</h5>' + errorstatuses;
                };
                $('#overallProgress').html(overallstats);
            } else {
                $('#overallProgress').html('There is no statistics');
            }
            self.stats = JSON.stringify(data.stats);
        } else if (data.error) {
            self.showError(data.error);
        };
    };
    EventReplayer.Control.prototype.onerror = function(evt) {
        var self = EventReplayer.control;
        console.log("websocket: onerror", evt);
    };
    EventReplayer.Control.prototype.showMessage = function(message) {
        $('#messageDialogHeader').text('Message');
        $('#messageDialogBodyContainer').removeClass('alert-error').addClass('alert-info');
        $('#messageDialogBody').text(message);
        $('#messageDialog').modal('show');
    };
    EventReplayer.Control.prototype.showError = function(message) {
        $('#messageDialogHeader').text('Error');
        $('#messageDialogBodyContainer').removeClass('alert-info').addClass('alert-error');
        $('#messageDialogBody').text(message);
        $('#messageDialog').modal('show');
    };
    EventReplayer.Control.prototype.setConnectionStatus = function(type, message) {
        $('#connectionStatus').text(message);
        $('#connectionStatus').removeClass('label-important')
                                .removeClass('label-warning')
                                .removeClass('label-success')
                                .addClass('label-' + type);
    };

    /* Workaround for bootstrap multi-modal bug */
    $.fn.modal.Constructor.prototype.enforceFocus = function () {};

    $(document).ready(function() {
        EventReplayer.control = new EventReplayer.Control();
        EventReplayer.control.init();
    });
})(jQuery);
