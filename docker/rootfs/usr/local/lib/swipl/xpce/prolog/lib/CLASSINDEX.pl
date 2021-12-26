/*  $Id$

    Creator: pce_make_library_index/1
    Purpose: Provide index of XPCE classes in directory
*/

class(auto_sized_picture, window, 'Window that automatically fits the contents', 'autowin.pl').
class(drag_and_drop_dict_item_gesture, drag_and_drop_gesture, 'Drag and drop items from a browser', 'dragdict.pl').
class(drag_and_drop_gesture, gesture, 'Drag and drop command-gesture', 'dragdrop.pl').
class(draw_shape_class, class, 'Handle class-level stuff', 'draw_extend.pl').
class(save_file, file, 'File as a destination (type-check only)', 'file_item.pl').
class(find_file_dialog, dialog, 'Browse for a file', 'find_file_dialog.pl').
class(finder, object, 'Find files on behalf of applications', 'find_file.pl').
class(passwd_item, text_item, 'text-item for entering a passwd', 'getpass.pl').
class(gradient, image, 'Create a gradient-image for filling', 'gradient.pl').
class(help_message_window, dialog, 'Window to display <-help_message', 'help_message.pl').
class(http_client, socket, 'Client socket for HTTP Protocol', 'http_client.pl').
class(partof_hyper, hyper, '<-to is a part of <-from', 'hyper.pl').
class(password_item, text_item, 'text-item for entering a passwd', 'password_item.pl').
class(arm, template, '(Un)arm objects in a window', 'pce_arm.pl').
class(arrow_item, label_box, 'Extry for viewing/editing an arrow-head', 'pce_arrow_item.pl').
class(colour_item, dialog_group, 'Item for selecting a colour', 'pce_colour_item.pl').
class(pce_config_editor, frame, 'library(pce_config): configuration editor', 'pce_configeditor.pl').
class(drag_and_drop_dict_item_gesture, drag_and_drop_gesture, 'Drag and drop items from a browser', 'pce_drag_and_drop_dict_item.pl').
class(drag_and_drop_gesture, gesture, 'Drag and drop command-gesture', 'pce_drag_and_drop.pl').
class(editable_text, text, 'Editable short graphical text', 'pce_editable_text.pl').
class(float_item, text_item, '', 'pce_float_item.pl').
class(font_item, label_box, 'Dialog item for defining a font', 'pce_font_item.pl').
class(graphical_browser, window, 'List-browser for graphicals', 'pce_graphical_browser.pl').
class(helper, sheet, 'Helper toplevel', 'pce_helper.pl').
class(history, object, 'Manage a location history', 'pce_history.pl').
class(identifier_item, text_item, 'Item for non-empty, canonicalised word', 'pce_identifier_item.pl').
class(image_browser, window, 'Browser for image files', 'pce_image_browser.pl').
class(image_item, label_box, 'Display and browse for an image', 'pce_image_item.pl').
class(progress_bar, label_box, 'Show progress to the user', 'pce_progress.pl').
class(reporter, label, 'Label for reporting', 'pce_report.pl').
class(select_set_item, dialog_group, 'Select objects from a set', 'pce_select_set_item.pl').
class(set_item, dialog_group, 'Edit a set of values', 'pce_set_item.pl').
class(style_item, figure, 'Item to define/edit a style object', 'pce_style_item.pl').
class(tagged_connection, connection, 'Connection with centered tag', 'pce_tagged_connection.pl').
class(template, object, 'use_class_template/1 super-class', 'pce_template.pl').
class(tick_box, menu, 'Simple boolean tick-box', 'pce_tick_box.pl').
class(toc_window, window, 'Window for table-of-contents', 'pce_toc.pl').
class(pce_unclip_window, window, '', 'pce_unclip.pl').
class(persistent_frame, frame, 'Frame remembering location', 'persistent_frame.pl').
class(print_graphics, template, 'Template defining ->print', 'print_graphics.pl').
class(prolog_predicate_item, text_item, 'Item for entering a Prolog predicate', 'prolog_predicate_item.pl').
class(prolog_predicate, object, 'Represent a Prolog predicate', 'prolog_predicate.pl').
class(scaled_bitmap, bitmap, 'Bitmap that scales its image', 'scaledbitmap.pl').
class(splash_screen, auto_sized_picture, 'Show splash window', 'splash_screen.pl').
class(tabbed_window, dialog, 'Resizeable window holding set of tabs', 'tabbed_window.pl').
class(tabular, device, 'Device with associated table <-layout_manager', 'tabular.pl').
class(toc_directory, toc_folder, 'Represent a directory', 'toc_filesystem.pl').
class(tool_bar, dialog_group, 'Row of buttons', 'toolbar.pl').
class(url_image, image, 'Image whose source comes from a URL', 'url_image.pl').
