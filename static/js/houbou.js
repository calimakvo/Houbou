$(document).ready(function() {
    $.ajaxSetup({
        headers: { 'X-CSRF-TOKEN': $('meta[name="csrf-token"]').attr('content') }
    });

    $('.hbDelBtn').on('click', function(event) {
        if(window.confirm('削除します、元にもどすことはできません、よろしいですか？')) {
            return true;
        } else {
            return false;
        }
    });

    $('.hbUserDelBtn').on('click', function(event) {
        if(window.confirm('削除します、よろしいですか？')) {
            return true;
        } else {
            return false;
        }
    });

    $('.btnStatusUpdate').on('click', function(event) {
        //通常のアクションをキャンセルする
        event.preventDefault();
        //Formの参照を取る
        $form = $(this).parents('form:first');
        $btn = $(this);
        
        $.ajax({
            url: $form.attr('action'),  // get form action
            type: $form.attr('method'), // get form method
            data: $form.serialize(),　  // form serialize
            timeout: 10000,
            beforeSend : function(xhr, settings) {
                //Buttonを無効にする
                $btn.attr('disabled' , true);
            },
            complete: function(xhr, textStatus){
                $btn.attr('disabled' , false);  
            },
            success: function (result, textStatus, xhr) {
                // ret = jQuery.parseJSON(result);
                //Alertで送信結果を表示する
                if(result){
                    if(result.result == 2) {
                        $btn.removeClass("btn-warning");
                        $btn.addClass("btn-info");
                        $btn.children("i").removeClass("fa-times");
                        $btn.children("i").addClass("fa-check");
                    } else if (result.result == 1) {
                        $btn.removeClass("btn-info");
                        $btn.addClass("btn-warning");
                        $btn.children("i").removeClass("fa-check");
                        $btn.children("i").addClass("fa-times");
                    } else {
                        alert(result.msg);
                    }
                    $form.find('input:hidden[name="version"]').val(result.version);
                }
            },
            error : function(data){
                $('#btnStatusUpdate').attr('disabled' , false);  
                console.debug(data);
            }
        });
    });

    $('.btnMediaInsert').on('click', function(event) {
        insTarget = document.getElementById(this.dataset.targetLinkId);
        window.open(
            '/hb-admin/mediains/1',
            'pop',
            'width=1000, height=600'
        );
        return false;
    });

    $('.hbPasteLink').on('click', function(event) {
        var ac = document.getElementById(this.dataset.linkId)
        var insTarget = window.opener.insTarget
        window.opener.onCallBack({"url": ac.href, target: insTarget});
        window.close();
    });
});

function onCallBack(data) {
    var url = data.url
    var area = data.target;
    var pos = area.selectionStart
    area.value = area.value.substr(0, pos)
		+ url
		+ area.value.substr(pos);
    area.focus();
    area.setSelectionRange(pos + url.length, pos + url.length);
}
